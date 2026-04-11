package com.denibertovic.todo.app.data.db

import androidx.room.Entity
import androidx.room.PrimaryKey

/**
 * One row per CRDT operation ever observed on this device.
 *
 * `payloadJson` stores the full operation JSON so we can replay it
 * into a [com.denibertovic.todo.core.crdt.Operation] without needing
 * a separate column per variant. The other columns are duplicated
 * out of the payload purely for indexing and filtering — they are
 * derived, not authoritative.
 *
 * `isPending` is true for operations generated locally that haven't
 * been acked by the server yet. Once sync completes successfully the
 * flag flips to false; we keep the row so the op log stays complete
 * for re-materialization.
 */
@Entity(tableName = "operations")
data class OperationEntity(
    @PrimaryKey val opId: String,
    val deviceId: String,
    val type: String,
    val itemId: String,
    val payloadJson: String,
    val createdAt: Long,
    val isPending: Boolean,
)

/**
 * Materialized view of the current todo set, rebuilt from the op log
 * after every sync. This is a cache — if it ever drifts from the
 * CRDT's canonical state we can drop it and recompute.
 *
 * `metadataJson` is a serialized `List<Metadata>` so the UI can
 * render project/context chips without replaying the whole op log.
 */
@Entity(tableName = "items_cache")
data class ItemCacheEntity(
    @PrimaryKey val itemId: String,
    val description: String,
    val priority: String?,
    val completed: Boolean,
    val deleted: Boolean,
    val createdAt: Long,
    val metadataJson: String,
)

/**
 * Key-value store for sync bookkeeping. Lives in the same database
 * as [OperationEntity] so that advancing the cursor and inserting
 * newly-received operations happens in a single transaction —
 * otherwise a crash between the two could desync us from the server.
 *
 * Well-known keys: `device_id`, `auth_token`, `cursor` (JSON),
 * `server_url`, `last_sync_at` (epoch ms as string), `device_name`.
 */
@Entity(tableName = "device_state")
data class DeviceStateEntity(
    @PrimaryKey val key: String,
    val value: String,
)
