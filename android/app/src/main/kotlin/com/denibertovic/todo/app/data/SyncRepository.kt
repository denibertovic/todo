package com.denibertovic.todo.app.data

import androidx.room.withTransaction
import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.data.net.SyncApi
import com.denibertovic.todo.core.crdt.Operation
import com.denibertovic.todo.core.crdt.SyncCursor
import com.denibertovic.todo.core.crdt.SyncRequest
import com.denibertovic.todo.core.crdt.materialize
import com.denibertovic.todo.core.json.TodoJson
import com.denibertovic.todo.core.types.DeviceId
import kotlinx.datetime.Clock

/**
 * The sync orchestrator — mirrors `Todo.Sync.Daemon.doSync` at
 * `src/Todo/Sync/Daemon.hs:249-296`. Outer flow:
 *
 * 1. Collect every local op that hasn't been acked yet.
 * 2. POST them to `/sync` together with the last known cursor.
 * 3. Drain every subsequent page (server-driven pagination) into a
 *    single in-memory batch.
 * 4. In one database transaction: insert the received ops, clear
 *    the pending flag on the ops we sent, rebuild the items cache
 *    from the full op log, and advance the cursor + last-sync
 *    timestamp.
 *
 * Step 4 is deliberately inside [withTransaction] so any crash
 * between dequeuing pending ops and ack'ing them can be retried
 * cleanly — the CRDT is idempotent, so seeing a handful of
 * operations twice is a no-op.
 */
class SyncRepository(
    private val db: TodoDatabase,
    private val apiProvider: (baseUrl: String) -> SyncApi = { SyncApi(it) },
) {

    sealed class Result {
        data class Success(val sent: Int, val received: Int) : Result()
        data object NotRegistered : Result()
        data class Failure(val error: Throwable) : Result()
    }

    suspend fun sync(): Result {
        val deviceStateDao = db.deviceStateDao()
        val opsDao = db.operationDao()
        val itemsDao = db.itemCacheDao()

        val serverUrl = deviceStateDao.get(DeviceStateKeys.SERVER_URL)
        val authToken = deviceStateDao.get(DeviceStateKeys.AUTH_TOKEN)
        val deviceIdStr = deviceStateDao.get(DeviceStateKeys.DEVICE_ID)
        if (serverUrl == null || authToken == null || deviceIdStr == null) {
            return Result.NotRegistered
        }
        val deviceId = DeviceId.parse(deviceIdStr)

        val api = apiProvider(serverUrl)
        try {
            val pendingEntities = opsDao.getPending()
            val pendingOps = pendingEntities.map { it.toOperation() }
            val initialCursor = deviceStateDao.get(DeviceStateKeys.CURSOR)?.let { decodeCursor(it) }

            // Paginate through every page the server has. Pending
            // ops only go on the first request; subsequent pages
            // carry an empty body plus the latest cursor.
            var currentCursor = initialCursor
            val received = mutableListOf<Operation>()
            var isFirst = true
            while (true) {
                val req = SyncRequest(
                    deviceId = deviceId,
                    cursor = currentCursor,
                    operations = if (isFirst) pendingOps else emptyList(),
                    authToken = null,
                )
                val resp = api.sync(authToken, req)
                received += resp.operations
                currentCursor = resp.cursor ?: currentCursor
                isFirst = false
                if (!resp.hasMore) break
            }

            db.withTransaction {
                val receivedEntities = received.map { it.toEntity(isPending = false) }
                opsDao.insertAll(receivedEntities)
                if (pendingEntities.isNotEmpty()) {
                    opsDao.clearPendingFlag(pendingEntities.map { it.opId })
                }
                rebuildItemsCache(deviceId)
                currentCursor?.let {
                    deviceStateDao.put(DeviceStateKeys.CURSOR, encodeCursor(it))
                }
                deviceStateDao.put(DeviceStateKeys.LAST_SYNC, Clock.System.now().toEpochMilliseconds().toString())
            }

            return Result.Success(sent = pendingOps.size, received = received.size)
        } catch (t: Throwable) {
            return Result.Failure(t)
        } finally {
            api.close()
        }
    }

    /**
     * Fold the complete op log into a fresh items-cache row set.
     * This is a full recompute every sync — at tens of thousands of
     * ops this is still well under a second on-device, and the
     * simplicity of a full recompute makes the caching story a lot
     * easier to reason about. If the op log ever grows past a
     * million rows we can switch to incremental application.
     */
    private suspend fun rebuildItemsCache(deviceId: DeviceId) {
        val opsDao = db.operationDao()
        val itemsDao = db.itemCacheDao()
        val allOps = opsDao.getAll().map { it.toOperation() }
        val materialized = materialize(deviceId, allOps)
        itemsDao.replaceAll(materialized.values.map { it.toCacheEntity() })
    }

    private fun encodeCursor(cursor: SyncCursor): String =
        TodoJson.encodeToString(SyncCursor.serializer(), cursor)

    private fun decodeCursor(encoded: String): SyncCursor? = runCatching {
        TodoJson.decodeFromString(SyncCursor.serializer(), encoded)
    }.getOrNull()
}
