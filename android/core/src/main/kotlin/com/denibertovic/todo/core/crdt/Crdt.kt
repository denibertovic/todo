package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Todo

/**
 * Apply a single operation to a [SyncState] and return the new state.
 *
 * Direct translation of `applyOperation` at
 * `src/Todo/Sync/CRDT.hs:43-94`. Preserves all of the CRDT's merge
 * rules:
 *
 * - **Add** merges conflicting fields via LWW on priority and
 *   description, and respects delete-wins and complete-wins on the
 *   boolean flags.
 * - **Complete** flips `completed` to true (never backtracks on its
 *   own — only a later [Operation.Uncomplete] can).
 * - **Uncomplete** flips `completed` to false.
 * - **Delete** flips `deleted` to true and is never reversed.
 * - **SetPriority** and **ModifyDescription** wrap the new value in
 *   an LWW register and merge against the current one.
 *
 * Operations targeting item IDs that have never been `Add`ed are
 * ignored (matching Haskell's `Map.adjust` semantics, which leaves
 * the map unchanged if the key is missing).
 */
fun SyncState.apply(op: Operation): SyncState {
    val newItems: Map<ItemId, SyncItem> = when (op) {
        is Operation.Add -> {
            val newItem = SyncItem(
                itemId = op.itemId,
                item = op.item,
                completed = false,
                deleted = false,
                priority = LWWRegister(op.item.priority, op.timestamp, op.deviceId),
                description = LWWRegister(op.item.description, op.timestamp, op.deviceId),
                createdAt = op.timestamp,
            )
            val existing = items[op.itemId]
            if (existing == null) {
                items + (op.itemId to newItem)
            } else {
                // Conflicting Add — merge LWW registers, respecting
                // delete-wins and complete-wins on booleans.
                val merged = existing.copy(
                    priority = newItem.priority.merge(existing.priority),
                    description = newItem.description.merge(existing.description),
                    completed = newItem.completed || existing.completed,
                    deleted = newItem.deleted || existing.deleted,
                )
                items + (op.itemId to merged)
            }
        }

        is Operation.Complete -> items[op.itemId]?.let { item ->
            items + (op.itemId to item.copy(completed = true))
        } ?: items

        is Operation.Uncomplete -> items[op.itemId]?.let { item ->
            items + (op.itemId to item.copy(completed = false))
        } ?: items

        is Operation.Delete -> items[op.itemId]?.let { item ->
            items + (op.itemId to item.copy(deleted = true))
        } ?: items

        is Operation.SetPriority -> items[op.itemId]?.let { item ->
            val newReg = LWWRegister(op.priority, op.timestamp, op.deviceId)
            items + (op.itemId to item.copy(priority = newReg.merge(item.priority)))
        } ?: items

        is Operation.ModifyDescription -> items[op.itemId]?.let { item ->
            val newReg = LWWRegister(op.description, op.timestamp, op.deviceId)
            items + (op.itemId to item.copy(description = newReg.merge(item.description)))
        } ?: items
    }

    return copy(items = newItems, operations = listOf(op) + operations)
}

/** Apply a batch of operations in order. */
fun SyncState.applyAll(ops: List<Operation>): SyncState =
    ops.fold(this) { st, op -> st.apply(op) }

/**
 * Deduplicate operations by [Operation.opId]. Operations are
 * commutative and idempotent, so dropping duplicates is always safe.
 * Mirrors `deduplicateOperations` at `src/Todo/Sync/CRDT.hs:102-103`.
 */
fun List<Operation>.dedupe(): List<Operation> {
    val seen = HashSet<com.denibertovic.todo.core.types.OperationId>(size)
    val result = ArrayList<Operation>(size)
    for (op in this) {
        if (seen.add(op.opId)) result += op
    }
    return result
}

/** Merge two operation lists, dropping duplicates. */
fun mergeOperations(a: List<Operation>, b: List<Operation>): List<Operation> =
    (a + b).dedupe()

/**
 * Fold an operation log into a materialized item map, starting from
 * empty state. Operations are sorted by timestamp first, so the
 * result is independent of delivery order — that's what makes the
 * CRDT commutative.
 *
 * Mirrors `materialize` at `src/Todo/Sync/CRDT.hs:107-114`.
 */
fun materialize(deviceId: DeviceId, ops: List<Operation>): Map<ItemId, SyncItem> {
    val sorted = ops.sortedBy { it.timestamp }
    return SyncState.empty(deviceId).applyAll(sorted).items
}

/**
 * Convert a materialized sync state to a flat list of [Todo] values,
 * skipping deleted items. Matches `materializeToTodos` at
 * `src/Todo/Sync/CRDT.hs:117-122`.
 *
 * The returned items use the LWW values for priority and
 * description, not the snapshot stored in [SyncItem.item] — that's
 * what lets remote edits to those fields show through after a sync.
 */
fun materializeToTodos(items: Map<ItemId, SyncItem>): List<Todo> =
    items.values
        .filter { !it.deleted }
        .map { it.toTodo() }

/** Convert a single [SyncItem] to a [Todo], folding the LWW registers back into the item. */
fun SyncItem.toTodo(): Todo {
    val merged = item.copy(
        priority = priority.value,
        description = description.value,
    )
    return if (completed) Todo.Completed(merged) else Todo.Incomplete(merged)
}
