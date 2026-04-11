package com.denibertovic.todo.app.data

import androidx.room.withTransaction
import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.sync.SyncScheduler
import com.denibertovic.todo.core.crdt.Operation
import com.denibertovic.todo.core.crdt.materialize
import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.OperationId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.TodoItem
import kotlinx.datetime.Clock

/**
 * User-facing mutation API. Every local action flows through here:
 *
 * 1. Build an [Operation] stamped with the current device ID and
 *    wall-clock time.
 * 2. Insert it into the op log with `isPending = true` so the next
 *    sync round will push it.
 * 3. Recompute the items cache in the same transaction so the UI
 *    sees the change immediately (optimistic rendering).
 * 4. Enqueue an expedited one-shot sync so the change lands on the
 *    server as quickly as the OS will let us.
 *
 * The alternative — updating the cache row directly — would require
 * duplicating the CRDT's merge logic on Android, which is exactly
 * the class of bug we want to avoid.
 */
class TodoActions(
    private val db: TodoDatabase,
    private val syncScheduler: SyncScheduler,
) {

    suspend fun add(item: TodoItem) {
        val itemId = ItemId.random()
        val op = Operation.Add(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
            item = item,
        )
        record(listOf(op))
    }

    suspend fun complete(itemId: ItemId) {
        record(listOf(Operation.Complete(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
        )))
    }

    suspend fun uncomplete(itemId: ItemId) {
        record(listOf(Operation.Uncomplete(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
        )))
    }

    suspend fun delete(itemId: ItemId) {
        record(listOf(Operation.Delete(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
        )))
    }

    suspend fun setPriority(itemId: ItemId, priority: Priority?) {
        record(listOf(Operation.SetPriority(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
            priority = priority,
        )))
    }

    suspend fun modifyDescription(itemId: ItemId, description: String) {
        record(listOf(Operation.ModifyDescription(
            opId = OperationId.random(),
            itemId = itemId,
            timestamp = Clock.System.now(),
            deviceId = deviceId(),
            description = description,
        )))
    }

    /**
     * Mark all currently-completed items as deleted. Equivalent to
     * the CLI's `archive` command at `src/Todo/Lib.hs:213-223`. On
     * Android we don't have a `done.txt` file, so archiving is just
     * a batch of delete operations.
     */
    suspend fun archiveCompleted() {
        val deviceId = deviceId()
        val now = Clock.System.now()
        val ops = db.operationDao().getAll()
            .map { it.toOperation() }
            .let { materialize(deviceId, it) }
            .values
            .filter { it.completed && !it.deleted }
            .map { item ->
                Operation.Delete(
                    opId = OperationId.random(),
                    itemId = item.itemId,
                    timestamp = now,
                    deviceId = deviceId,
                )
            }
        if (ops.isNotEmpty()) record(ops)
    }

    /**
     * Insert a batch of locally-created ops, rebuild the cache, and
     * kick off a sync round. All state mutation happens inside a
     * single DB transaction so the UI can never observe a
     * half-applied batch.
     */
    private suspend fun record(ops: List<Operation>) {
        val deviceId = deviceId()
        db.withTransaction {
            db.operationDao().insertAll(ops.map { it.toEntity(isPending = true) })
            val allOps = db.operationDao().getAll().map { it.toOperation() }
            val items = materialize(deviceId, allOps)
            db.itemCacheDao().replaceAll(items.values.map { it.toCacheEntity() })
        }
        syncScheduler.enqueueExpeditedSync()
    }

    private suspend fun deviceId(): DeviceId {
        val text = db.deviceStateDao().get(DeviceStateKeys.DEVICE_ID)
            ?: error("TodoActions invoked before device registration completed")
        return DeviceId.parse(text)
    }
}
