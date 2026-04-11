package com.denibertovic.todo.app.data

import android.content.Context
import android.net.Uri
import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.core.crdt.materialize
import com.denibertovic.todo.core.crdt.materializeToTodos
import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.Todo
import com.denibertovic.todo.core.types.renderFile
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

/**
 * Disaster-recovery escape hatch for rebuilding `~/todo.txt` from
 * the phone if the user's laptop dies and the canonical file is
 * lost. Exposes two export modes:
 *
 * - `todo.txt`: incomplete items only
 * - `done.txt`: completed (but not deleted) items only
 *
 * Materialization runs against the full op log rather than the
 * cache so the exported file is guaranteed to match what the CRDT
 * says the canonical state is, with no chance of cache drift.
 */
class ExportRepository(
    private val db: TodoDatabase,
) {

    /** Which subset of items to export. */
    enum class Mode { Active, Done }

    /**
     * Materialize the current state and render it as todo.txt file
     * contents. Returns the exact bytes that would go on disk,
     * including the trailing newline.
     */
    suspend fun exportBytes(mode: Mode): ByteArray = withContext(Dispatchers.IO) {
        val deviceIdStr = db.deviceStateDao().get(DeviceStateKeys.DEVICE_ID)
            ?: error("Cannot export without a registered device")
        val deviceId = DeviceId.parse(deviceIdStr)
        val ops = db.operationDao().getAll().map { it.toOperation() }
        val items = materialize(deviceId, ops)
        val allTodos = materializeToTodos(items)
        val filtered = when (mode) {
            Mode.Active -> allTodos.filter { it is Todo.Incomplete }
            Mode.Done -> allTodos.filter { it is Todo.Completed }
        }
        filtered.renderFile().toByteArray(Charsets.UTF_8)
    }

    /**
     * Write the export to a user-chosen [Uri] via the Storage Access
     * Framework. Caller is responsible for launching the SAF file
     * picker — this method assumes the URI is already granted.
     */
    suspend fun writeTo(context: Context, uri: Uri, mode: Mode) = withContext(Dispatchers.IO) {
        val bytes = exportBytes(mode)
        context.contentResolver.openOutputStream(uri, "wt")?.use { out ->
            out.write(bytes)
            out.flush()
        } ?: error("Unable to open output stream for $uri")
    }
}
