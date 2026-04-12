package com.denibertovic.todo.app.ui.settings

import android.content.Context
import android.net.Uri
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.ExportRepository
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.sync.SyncScheduler
import kotlinx.coroutines.launch

class SettingsViewModel(
    private val database: TodoDatabase,
    private val syncRepository: SyncRepository,
    private val syncScheduler: SyncScheduler,
    private val exportRepository: ExportRepository,
) : ViewModel() {

    var serverUrl by mutableStateOf("")
        private set
    var deviceName by mutableStateOf("")
        private set
    var lastSync by mutableStateOf<Long?>(null)
        private set
    var pendingCount by mutableIntStateOf(0)
        private set
    var syncing by mutableStateOf(false)
        private set
    var syncResult by mutableStateOf<String?>(null)
        private set

    init {
        viewModelScope.launch {
            val dao = database.deviceStateDao()
            serverUrl = dao.get(DeviceStateKeys.SERVER_URL).orEmpty()
            deviceName = dao.get(DeviceStateKeys.DEVICE_NAME).orEmpty()
            lastSync = dao.get(DeviceStateKeys.LAST_SYNC)?.toLongOrNull()
            pendingCount = database.operationDao().pendingCount()
        }
    }

    fun sync() {
        syncing = true
        syncResult = null
        viewModelScope.launch {
            try {
                val result = syncRepository.sync()
                syncResult = when (result) {
                    is SyncRepository.Result.Success ->
                        "Sent ${result.sent}, received ${result.received}"
                    is SyncRepository.Result.NotRegistered ->
                        "Not registered"
                    is SyncRepository.Result.Failure ->
                        "Sync failed: ${result.error.message}"
                }
                lastSync = database.deviceStateDao()
                    .get(DeviceStateKeys.LAST_SYNC)?.toLongOrNull()
                pendingCount = database.operationDao().pendingCount()
            } catch (e: Exception) {
                syncResult = "Sync failed: ${e.message}"
            } finally {
                syncing = false
            }
        }
    }

    fun exportTo(context: Context, uri: Uri, mode: ExportRepository.Mode) {
        viewModelScope.launch {
            try {
                exportRepository.writeTo(context, uri, mode)
            } catch (e: Exception) {
                syncResult = "Export failed: ${e.message}"
            }
        }
    }

    fun wipeLocalState(onWiped: () -> Unit) {
        viewModelScope.launch {
            try {
                syncScheduler.cancelAll()
                database.clearAllTables()
                database.deviceStateDao().clear()
                TodoDatabase.reset()
                onWiped()
            } catch (e: Exception) {
                syncResult = "Wipe failed: ${e.message}"
            }
        }
    }

    class Factory(private val app: TodoApplication) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            SettingsViewModel(
                app.database, app.syncRepository, app.syncScheduler, app.exportRepository,
            ) as T
    }
}
