package com.denibertovic.todo.core.types

import com.denibertovic.todo.core.crdt.LWWRegister
import com.denibertovic.todo.core.crdt.Operation
import com.denibertovic.todo.core.crdt.SyncRequest
import com.denibertovic.todo.core.crdt.SyncResponse
import com.denibertovic.todo.core.json.TodoJson
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import kotlinx.datetime.Instant
import kotlinx.datetime.LocalDate
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

/**
 * Structural JSON tests. These do NOT compare strings — string
 * comparison is too brittle given key-ordering differences between
 * Aeson and kotlinx.serialization. Instead, we parse both sides back
 * into [JsonObject]s and assert they match structurally. That's the
 * same equivalence the server uses when comparing wire formats.
 */
@OptIn(ExperimentalUuidApi::class)
class JsonTest : DescribeSpec({

    // A relaxed Json instance used only for parsing fixture strings
    // back into JsonObject for structural comparison.
    val anyJson = Json { ignoreUnknownKeys = true }

    fun jsonStructurallyEquals(a: String, b: String) {
        anyJson.parseToJsonElement(a) shouldBe anyJson.parseToJsonElement(b)
    }

    describe("Priority") {
        it("serializes as a single letter") {
            TodoJson.encodeToString<Priority>(Priority.A) shouldBe "\"A\""
            TodoJson.encodeToString<Priority>(Priority.Z) shouldBe "\"Z\""
        }

        it("deserializes from a single letter") {
            TodoJson.decodeFromString<Priority>("\"B\"") shouldBe Priority.B
        }
    }

    describe("TodoItem") {
        it("round-trips with snake_case date fields") {
            val item = TodoItem(
                priority = Priority.A,
                description = "buy milk",
                metadata = listOf(Metadata.ProjectM(Project("groceries"))),
                createdAt = LocalDate(2024, 3, 15),
                completedAt = null,
            )
            val encoded = TodoJson.encodeToString(item)
            jsonStructurallyEquals(
                encoded,
                """
                {
                  "priority": "A",
                  "description": "buy milk",
                  "metadata": [ {"type": "project", "project": "groceries"} ],
                  "created_at": "2024-03-15",
                  "completed_at": null
                }
                """.trimIndent()
            )
            val decoded = TodoJson.decodeFromString<TodoItem>(encoded)
            decoded shouldBe item
        }
    }

    describe("Metadata variants") {
        it("serializes Tag KeyValue with type discriminator") {
            val md: Metadata = Metadata.TagM(Tag.KeyValue("k", "v"))
            val encoded = TodoJson.encodeToString(md)
            jsonStructurallyEquals(
                encoded,
                """
                {
                  "type": "tag",
                  "tag": { "type": "tag", "key": "k", "value": "v" }
                }
                """.trimIndent()
            )
        }

        it("serializes due_next as an empty object with discriminator") {
            val md: Metadata = Metadata.TagM(Tag.Next)
            val encoded = TodoJson.encodeToString(md)
            jsonStructurallyEquals(
                encoded,
                """
                { "type": "tag", "tag": { "type": "due_next" } }
                """.trimIndent()
            )
        }

        it("serializes String variant under the 'value' field") {
            val md: Metadata = Metadata.Str("foo")
            val encoded = TodoJson.encodeToString(md)
            jsonStructurallyEquals(
                encoded,
                """{ "type": "string", "value": "foo" }"""
            )
        }
    }

    describe("Todo status wrapper") {
        it("uses 'status' as discriminator, not 'type'") {
            val todo: Todo = Todo.Incomplete(TodoItem(description = "x"))
            val encoded = TodoJson.encodeToString(todo)
            val root = anyJson.parseToJsonElement(encoded).let { it as JsonObject }
            root.containsKey("status") shouldBe true
            root.containsKey("item") shouldBe true
        }
    }

    describe("Operation") {
        it("emits flat common fields + type discriminator + op-specific fields") {
            val op: Operation = Operation.Add(
                opId = OperationId(Uuid.parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")),
                itemId = ItemId(Uuid.parse("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")),
                timestamp = Instant.parse("2026-04-11T00:00:00Z"),
                deviceId = DeviceId(Uuid.parse("cccccccc-cccc-cccc-cccc-cccccccccccc")),
                item = TodoItem(description = "hello"),
            )
            val encoded = TodoJson.encodeToString(op)
            val obj = anyJson.parseToJsonElement(encoded) as JsonObject
            obj["type"].toString() shouldBe "\"add\""
            obj.containsKey("op_id") shouldBe true
            obj.containsKey("item_id") shouldBe true
            obj.containsKey("device_id") shouldBe true
            obj.containsKey("timestamp") shouldBe true
            obj.containsKey("item") shouldBe true
        }
    }

    describe("SyncRequest / SyncResponse") {
        it("round-trips empty sync request") {
            val req = SyncRequest(
                deviceId = DeviceId(Uuid.parse("cccccccc-cccc-cccc-cccc-cccccccccccc")),
                cursor = null,
                operations = emptyList(),
                authToken = null,
            )
            TodoJson.decodeFromString<SyncRequest>(TodoJson.encodeToString(req)) shouldBe req
        }

        it("round-trips an empty sync response") {
            val resp = SyncResponse(operations = emptyList(), cursor = null, hasMore = false)
            TodoJson.decodeFromString<SyncResponse>(TodoJson.encodeToString(resp)) shouldBe resp
        }
    }

    describe("LWWRegister<T>") {
        it("round-trips with a string value") {
            val reg = LWWRegister(
                value = "hi",
                timestamp = Instant.parse("2026-04-11T00:00:00Z"),
                deviceId = DeviceId(Uuid.parse("dddddddd-dddd-dddd-dddd-dddddddddddd")),
            )
            val encoded = TodoJson.encodeToString(reg)
            TodoJson.decodeFromString<LWWRegister<String>>(encoded) shouldBe reg
        }
    }
})
