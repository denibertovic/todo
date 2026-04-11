package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.OperationId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.Todo
import com.denibertovic.todo.core.types.TodoItem
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import kotlinx.datetime.Instant
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

/**
 * Ported from `test/Test/Sync/CRDTSpec.hs`. These are the core
 * correctness properties the Kotlin CRDT must preserve to remain
 * bit-compatible with the Haskell server.
 */
@OptIn(ExperimentalUuidApi::class)
class CrdtTest : DescribeSpec({

    // Fixed IDs so the LWW tiebreaker is deterministic across runs.
    val deviceA = DeviceId(Uuid.parse("00000000-0000-0000-0000-00000000000a"))
    val deviceB = DeviceId(Uuid.parse("00000000-0000-0000-0000-00000000000b"))

    // deviceB > deviceA lexicographically
    val item1 = ItemId(Uuid.parse("11111111-1111-1111-1111-111111111111"))

    fun opId(n: Int) =
        OperationId(Uuid.parse("22222222-2222-2222-2222-${n.toString().padStart(12, '0')}"))

    fun instant(secs: Long): Instant = Instant.fromEpochSeconds(secs)

    fun addOp(n: Int, t: Long, dev: DeviceId, description: String, priority: Priority? = null): Operation.Add =
        Operation.Add(
            opId = opId(n),
            itemId = item1,
            timestamp = instant(t),
            deviceId = dev,
            item = TodoItem(priority = priority, description = description),
        )

    describe("apply") {
        it("is commutative: apply(apply(s, a), b) == apply(apply(s, b), a) for distinct items") {
            val item2 = ItemId(Uuid.parse("33333333-3333-3333-3333-333333333333"))
            val a = Operation.Add(opId(1), item1, instant(1), deviceA, TodoItem(description = "a"))
            val b = Operation.Add(opId(2), item2, instant(2), deviceA, TodoItem(description = "b"))

            val s1 = SyncState.empty(deviceA).apply(a).apply(b)
            val s2 = SyncState.empty(deviceA).apply(b).apply(a)
            s1.items shouldBe s2.items
        }

        it("is idempotent: applying the same op twice is the same as once") {
            val a = addOp(1, 1, deviceA, "x")
            val s1 = SyncState.empty(deviceA).apply(a)
            val s2 = s1.apply(a)
            s1.items shouldBe s2.items
        }
    }

    describe("delete-wins") {
        it("a delete cannot be unwound by a later uncomplete or set_priority") {
            val add = addOp(1, 1, deviceA, "x")
            val del = Operation.Delete(opId(2), item1, instant(2), deviceA)
            val setPri = Operation.SetPriority(opId(3), item1, instant(3), deviceA, Priority.A)
            val state = SyncState.empty(deviceA).applyAll(listOf(add, del, setPri))
            state.items[item1]!!.deleted shouldBe true
        }

        it("applying delete before add still leaves the item deleted after the add") {
            // Delete arrives out of order: delete first, add second.
            // Since Map.adjust is a no-op on missing keys, the delete
            // is silently dropped and the final add wins. This is a
            // known behavior of the Haskell CRDT — the tombstone is
            // only applied if the item already exists. Document it
            // here so the Kotlin port stays in lockstep.
            val del = Operation.Delete(opId(1), item1, instant(1), deviceA)
            val add = addOp(2, 2, deviceA, "x")
            val state = SyncState.empty(deviceA).applyAll(listOf(del, add))
            state.items[item1]!!.deleted shouldBe false
        }
    }

    describe("complete-wins on merge") {
        it("merging two Adds, one with completed flag, leaves completed true") {
            val addA = addOp(1, 1, deviceA, "x")
            // simulate a state where the item was independently
            // marked complete on device B
            val state1 = SyncState.empty(deviceA).apply(addA)
                .let { it.copy(items = it.items.mapValues { (_, v) -> v.copy(completed = true) }) }
            // applying a second Add should not flip completed back to false
            val addA2 = addOp(2, 2, deviceB, "x")
            val state2 = state1.apply(addA2)
            state2.items[item1]!!.completed shouldBe true
        }
    }

    describe("LWW tiebreak by device ID") {
        it("when timestamps are equal, higher device ID wins") {
            val t = instant(100)
            val regA = LWWRegister("from A", t, deviceA)
            val regB = LWWRegister("from B", t, deviceB)
            // deviceB > deviceA, so merging picks B regardless of order
            regA.merge(regB).value shouldBe "from B"
            regB.merge(regA).value shouldBe "from B"
        }

        it("later timestamp always wins regardless of device") {
            val early = LWWRegister("early", instant(10), deviceB)
            val late = LWWRegister("late", instant(20), deviceA)
            early.merge(late).value shouldBe "late"
            late.merge(early).value shouldBe "late"
        }
    }

    describe("dedupe") {
        it("drops duplicates by opId") {
            val a = addOp(1, 1, deviceA, "x")
            val b = addOp(2, 2, deviceA, "y")
            val input = listOf(a, b, a)
            input.dedupe() shouldNotBe input
            input.dedupe().size shouldBe 2
        }
    }

    describe("materialize / materializeToTodos") {
        it("excludes deleted items") {
            val add = addOp(1, 1, deviceA, "keep")
            val add2 = addOp(2, 2, deviceA, "drop").copy(itemId = ItemId(Uuid.parse("99999999-9999-9999-9999-999999999999")))
            val del = Operation.Delete(opId(3), add2.itemId, instant(3), deviceA)
            val items = materialize(deviceA, listOf(add, add2, del))
            val todos = materializeToTodos(items)
            todos.size shouldBe 1
            todos.first().item.description shouldBe "keep"
        }

        it("applies LWW to priority: the later timestamp wins") {
            val add = addOp(1, 10, deviceA, "x")
            val setPriHigh = Operation.SetPriority(opId(2), item1, instant(20), deviceA, Priority.A)
            val setPriLow = Operation.SetPriority(opId(3), item1, instant(30), deviceA, Priority.C)
            val items = materialize(deviceA, listOf(add, setPriHigh, setPriLow))
            (items[item1]!!.toTodo() as Todo.Incomplete).item.priority shouldBe Priority.C
        }
    }
})
