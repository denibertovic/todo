package com.denibertovic.todo.app.ui.onboarding

import android.os.Build
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.QrCodeScanner
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.PrefilledRegistration
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.RegistrationRepository
import kotlinx.coroutines.launch

/**
 * First-run onboarding: server URL + invite code. Three ways to
 * populate the fields:
 *
 * 1. Manually type them.
 * 2. Scan the invite QR code using the in-app scanner
 *    ([QrScannerScreen]) — triggered by the "Scan QR" button. This
 *    is the path most users will take.
 * 3. Open a `todo-sync://register?server=...&code=...` URL from
 *    outside the app (e.g. by scanning a QR with the system camera
 *    app); [MainActivity] hands that intent back as
 *    [PrefilledRegistration] and we pre-fill the form on launch.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun OnboardingScreen(
    app: TodoApplication,
    prefilled: PrefilledRegistration?,
    onPrefillConsumed: () -> Unit,
    onRegistered: () -> Unit,
) {
    var serverUrl by remember { mutableStateOf(prefilled?.serverUrl ?: "https://") }
    var inviteCode by remember { mutableStateOf(prefilled?.inviteCode ?: "") }
    var deviceName by remember { mutableStateOf(defaultDeviceName()) }
    var inflight by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }
    var showScanner by remember { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

    LaunchedEffect(prefilled) {
        if (prefilled != null) onPrefillConsumed()
    }

    // Full-screen scanner overlay. Closes itself either on user
    // cancel or on a successful scan — on success we copy whatever
    // the scanner parsed out into the form fields and let the user
    // hit Register to confirm. We never auto-submit; a misfire on a
    // wall poster shouldn't start a remote handshake.
    if (showScanner) {
        QrScannerScreen(
            onCancel = { showScanner = false },
            onScanned = { result ->
                showScanner = false
                result.serverUrl?.takeIf { it.isNotBlank() }?.let { serverUrl = it }
                result.inviteCode?.takeIf { it.isNotBlank() }?.let { inviteCode = it }
                if (result.serverUrl == null && result.inviteCode == null) {
                    error = "Scanned QR was not a recognized invite code. Got: ${result.rawText.take(80)}"
                } else {
                    error = null
                }
            },
        )
        return
    }

    Scaffold(
        topBar = { TopAppBar(title = { Text("Connect to sync server") }) },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Paste the server URL and invite code you generated on your laptop, or scan the QR code.")

            OutlinedButton(
                onClick = { showScanner = true },
                modifier = Modifier.fillMaxWidth(),
            ) {
                Row(
                    verticalAlignment = androidx.compose.ui.Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    Icon(Icons.Default.QrCodeScanner, contentDescription = null)
                    Text("Scan QR")
                }
            }

            OutlinedTextField(
                value = serverUrl,
                onValueChange = { serverUrl = it },
                label = { Text("Server URL") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            OutlinedTextField(
                value = inviteCode,
                onValueChange = { inviteCode = it },
                label = { Text("Invite code") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            OutlinedTextField(
                value = deviceName,
                onValueChange = { deviceName = it },
                label = { Text("Device name") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            error?.let {
                Text(it, color = MaterialTheme.colorScheme.error)
            }
            Spacer(Modifier.height(8.dp))
            Button(
                enabled = !inflight && serverUrl.startsWith("http") && inviteCode.isNotBlank(),
                onClick = {
                    inflight = true
                    error = null
                    scope.launch {
                        val result = app.registrationRepository.register(
                            serverUrl = serverUrl.trim().trimEnd('/'),
                            inviteCode = inviteCode.trim(),
                            deviceName = deviceName.ifBlank { defaultDeviceName() },
                        )
                        inflight = false
                        when (result) {
                            is RegistrationRepository.Result.Success -> onRegistered()
                            is RegistrationRepository.Result.Failure ->
                                error = "Registration failed: ${result.error.message}"
                        }
                    }
                },
                modifier = Modifier.fillMaxWidth(),
            ) {
                Text(if (inflight) "Registering…" else "Register")
            }
        }
    }
}

private fun defaultDeviceName(): String {
    val model = Build.MODEL ?: "android"
    val manufacturer = Build.MANUFACTURER ?: ""
    return if (manufacturer.isBlank() || model.startsWith(manufacturer, ignoreCase = true)) {
        model
    } else {
        "$manufacturer $model"
    }
}
