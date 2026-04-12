package com.denibertovic.todo.app.data

import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.db.OperationEntity
import com.denibertovic.todo.core.crdt.Operation
import com.denibertovic.todo.core.crdt.SyncItem
import com.denibertovic.todo.core.json.TodoJson
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.parser.TodoParser
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.TodoItem
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.encodeToString

/**
 * Bridge between the pure-Kotlin [Operation] / [SyncItem] model in
 * `:core` and the Room-friendly `*Entity` rows in the `:app/data/db`
 * layer. The database stores [Operation]s as opaque JSON blobs in
 * `payloadJson` and pulls out a few denormalized columns (op ID,
 * type, item ID, timestamp) for indexing — that way we only need to
 * know about [Operation] variants in one place (`:core/crdt/Operation.kt`).
 */

private val metadataListSerializer = ListSerializer(Metadata.serializer())

fun Operation.toEntity(isPending: Boolean): OperationEntity = OperationEntity(
    opId = opId.toString(),
    deviceId = deviceId.toString(),
    type = when (this) {
        is Operation.Add -> "add"
        is Operation.Complete -> "complete"
        is Operation.Uncomplete -> "uncomplete"
        is Operation.Delete -> "delete"
        is Operation.SetPriority -> "set_priority"
        is Operation.ModifyDescription -> "modify_description"
    },
    itemId = itemId.toString(),
    payloadJson = TodoJson.encodeToString(Operation.serializer(), this),
    createdAt = timestamp.toEpochMilliseconds(),
    isPending = isPending,
)

fun OperationEntity.toOperation(): Operation =
    TodoJson.decodeFromString(Operation.serializer(), payloadJson)

fun SyncItem.toCacheEntity(): ItemCacheEntity = ItemCacheEntity(
    itemId = itemId.toString(),
    description = description.value,
    priority = priority.value?.name,
    completed = completed,
    deleted = deleted,
    createdAt = createdAt.toEpochMilliseconds(),
    metadataJson = TodoJson.encodeToString(metadataListSerializer, item.metadata),
)

fun ItemCacheEntity.decodeMetadata(): List<Metadata> =
    if (metadataJson.isBlank()) emptyList()
    else TodoJson.decodeFromString(metadataListSerializer, metadataJson)

fun ItemCacheEntity.toTodoItem(): TodoItem = TodoItem(
    priority = priority?.firstOrNull()?.let { Priority.fromLetter(it) },
    description = description,
    metadata = decodeMetadata(),
)

/**
 * Extract the plain-text description from a cache entity, stripping any
 * metadata tokens that may have been inlined by the edit screen. For items
 * that have never been edited the description is already clean.
 */
fun ItemCacheEntity.displayDescription(): String {
    val line = buildString {
        priority?.let { append("($it) ") }
        append(description)
    }
    return TodoParser.parseLine(line).getOrNull()?.item?.description ?: description
}

/**
 * Resolve the current metadata for a cache entity. When an item has been
 * edited, metadata tokens live inside the description string (the CRDT has
 * no dedicated metadata-update operation). We parse the description first
 * and fall back to the original [metadataJson] from the Add operation.
 */
fun ItemCacheEntity.displayMetadata(): List<Metadata> {
    val line = buildString {
        priority?.let { append("($it) ") }
        append(description)
    }
    val inlineMeta = TodoParser.parseLine(line).getOrNull()?.item?.metadata ?: emptyList()
    return inlineMeta.ifEmpty { decodeMetadata() }
}
