package com.denibertovic.todo.app.ui.settings

import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.ExportRepository
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import kotlinx.coroutines.launch

/**
 * Settings screen. Exposes:
 * - Server URL and device name (read-only for now; changing them
 *   requires re-running onboarding against a new invite code).
 * - Last-sync timestamp and pending op count.
 * - "Sync now" button that hands off to [SyncRepository.sync] and
 *   surfaces errors inline.
 * - "Export todo.txt" / "Export done.txt" buttons wired into the
 *   Storage Access Framework — the disaster-recovery escape hatch
 *   described in Phase 4.6.
 * - "Wipe local state" destructive action with a confirmation
 *   dialog: clears Room, forces re-onboarding.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SettingsScreen(
    app: TodoApplication,
    onBack: () -> Unit,
    onWiped: () -> Unit,
) {
    val scope = rememberCoroutineScope()
    val context = LocalContext.current

    var serverUrl by remember { mutableStateOf("") }
    var deviceName by remember { mutableStateOf("") }
    var lastSync by remember { mutableStateOf<Long?>(null) }
    var pending by remember { mutableStateOf(0) }
    var syncing by remember { mutableStateOf(false) }
    var syncResult by remember { mutableStateOf<String?>(null) }
    var showWipeConfirm by remember { mutableStateOf(false) }

    LaunchedEffect(Unit) {
        val dao = app.database.deviceStateDao()
        serverUrl = dao.get(DeviceStateKeys.SERVER_URL).orEmpty()
        deviceName = dao.get(DeviceStateKeys.DEVICE_NAME).orEmpty()
        lastSync = dao.get(DeviceStateKeys.LAST_SYNC)?.toLongOrNull()
        pending = app.database.operationDao().pendingCount()
    }

    // SAF export launchers — one for the active `todo.txt` file and
    // one for `done.txt`. The user picks a destination in the system
    // file picker and we write the rendered bytes into the returned
    // URI.
    val exportTodo = rememberLauncherForActivityResult(
        contract = ActivityResultContracts.CreateDocument("text/plain"),
    ) { uri ->
        if (uri != null) {
            scope.launch {
                app.exportRepository.writeTo(context, uri, ExportRepository.Mode.Active)
            }
        }
    }
    val exportDone = rememberLauncherForActivityResult(
        contract = ActivityResultContracts.CreateDocument("text/plain"),
    ) { uri ->
        if (uri != null) {
            scope.launch {
                app.exportRepository.writeTo(context, uri, ExportRepository.Mode.Done)
            }
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Settings") },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Back")
                    }
                },
            )
        },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Server", style = MaterialTheme.typography.titleMedium)
            Text(serverUrl.ifBlank { "— not configured —" })
            Text("Device", style = MaterialTheme.typography.titleMedium)
            Text(deviceName.ifBlank { "— default —" })

            Spacer(Modifier.height(8.dp))
            Text("Last sync", style = MaterialTheme.typography.titleMedium)
            Text(lastSync?.let { formatEpoch(it) } ?: "never")
            Text("Pending operations: $pending")
            syncResult?.let { Text(it, color = MaterialTheme.colorScheme.secondary) }

            Spacer(Modifier.height(8.dp))
            Button(
                onClick = {
                    syncing = true
                    syncResult = null
                    scope.launch {
                        val result = app.syncRepository.sync()
                        syncing = false
                        syncResult = when (result) {
                            is SyncRepository.Result.Success ->
                                "Sent ${result.sent}, received ${result.received}"
                            is SyncRepository.Result.NotRegistered ->
                                "Not registered"
                            is SyncRepository.Result.Failure ->
                                "Sync failed: ${result.error.message}"
                        }
                        lastSync = app.database.deviceStateDao()
                            .get(DeviceStateKeys.LAST_SYNC)?.toLongOrNull()
                        pending = app.database.operationDao().pendingCount()
                    }
                },
                enabled = !syncing,
                modifier = Modifier.fillMaxWidth(),
            ) { Text(if (syncing) "Syncing…" else "Sync now") }

            Spacer(Modifier.height(16.dp))
            Text(
                "Disaster recovery",
                style = MaterialTheme.typography.titleMedium,
            )
            Text(
                "Export a plain todo.txt file you can drop into ~/todo.txt on your laptop if the original is lost.",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            OutlinedButton(
                onClick = { exportTodo.launch("todo.txt") },
                modifier = Modifier.fillMaxWidth(),
            ) { Text("Export todo.txt") }
            OutlinedButton(
                onClick = { exportDone.launch("done.txt") },
                modifier = Modifier.fillMaxWidth(),
            ) { Text("Export done.txt") }

            Spacer(Modifier.height(24.dp))
            Button(
                onClick = { showWipeConfirm = true },
                colors = ButtonDefaults.buttonColors(
                    containerColor = MaterialTheme.colorScheme.error,
                ),
                modifier = Modifier.fillMaxWidth(),
            ) { Text("Wipe local state") }
        }
    }

    if (showWipeConfirm) {
        AlertDialog(
            onDismissRequest = { showWipeConfirm = false },
            title = { Text("Wipe local state?") },
            text = {
                Text(
                    "This deletes the operation log, cached items, and sync credentials. " +
                        "You will need to re-register this device with a fresh invite code.",
                )
            },
            confirmButton = {
                TextButton(
                    onClick = {
                        showWipeConfirm = false
                        scope.launch {
                            app.syncScheduler.cancelAll()
                            val db = app.database
                            db.clearAllTables()
                            db.deviceStateDao().clear()
                            TodoDatabase.reset()
                            onWiped()
                        }
                    },
                ) { Text("Wipe", color = MaterialTheme.colorScheme.error) }
            },
            dismissButton = {
                TextButton(onClick = { showWipeConfirm = false }) { Text("Cancel") }
            },
        )
    }
}

private fun formatEpoch(epochMs: Long): String {
    val instant = java.util.Date(epochMs)
    return instant.toString()
}
