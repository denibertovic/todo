package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.TodoItem
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * A synchronized todo item together with its CRDT metadata.
 *
 * Wire shape matches `SyncItem` at `src/Todo/Sync/Types.hs:247-258`.
 * The `item` field is the last-added snapshot; `priority` and
 * `description` are LWW registers that may diverge from the snapshot
 * after remote edits.
 */
@Serializable
data class SyncItem(
    @SerialName("siItemId") val itemId: ItemId,
    @SerialName("siItem") val item: TodoItem,
    @SerialName("siCompleted") val completed: Boolean,
    @SerialName("siDeleted") val deleted: Boolean,
    @SerialName("siPriority") val priority: LWWRegister<Priority?>,
    @SerialName("siDescription") val description: LWWRegister<String>,
    @SerialName("siCreatedAt") val createdAt: Instant,
)

/**
 * Pagination cursor for the `/sync` endpoint. The server uses a
 * composite `(timestamp, op_id)` cursor so that many operations with
 * the same insertion timestamp can still be paginated deterministically.
 *
 * Mirrors `SyncCursor` at `src/Todo/Sync/Types.hs:308-311`.
 */
@Serializable
data class SyncCursor(
    @SerialName("timestamp") val timestamp: Instant,
    @SerialName("op_id") val opId: com.denibertovic.todo.core.types.OperationId,
)

/**
 * In-memory state used by the CRDT fold. Not persisted directly on
 * Android (we store the operation log in Room and rebuild this on
 * demand), but kept here so the Kotlin CRDT has the same shape as
 * the Haskell reference implementation in
 * `src/Todo/Sync/Types.hs:266-304`.
 */
data class SyncState(
    val items: Map<ItemId, SyncItem> = emptyMap(),
    val operations: List<Operation> = emptyList(),
    val serverCursor: SyncCursor? = null,
    val deviceId: DeviceId,
    val pendingOps: List<Operation> = emptyList(),
) {
    companion object {
        fun empty(deviceId: DeviceId): SyncState = SyncState(deviceId = deviceId)
    }
}
