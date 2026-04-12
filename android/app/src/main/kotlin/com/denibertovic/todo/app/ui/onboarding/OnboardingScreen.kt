package com.denibertovic.todo.app.ui.onboarding

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
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun OnboardingScreen(
    viewModel: OnboardingViewModel,
    onRegistered: () -> Unit,
) {
    // Full-screen scanner overlay
    if (viewModel.showScanner) {
        QrScannerScreen(
            onCancel = { viewModel.showScanner = false },
            onScanned = viewModel::onScanResult,
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
                onClick = { viewModel.showScanner = true },
                modifier = Modifier.fillMaxWidth(),
            ) {
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    Icon(Icons.Default.QrCodeScanner, contentDescription = null)
                    Text("Scan QR")
                }
            }

            OutlinedTextField(
                value = viewModel.serverUrl,
                onValueChange = { viewModel.serverUrl = it },
                label = { Text("Server URL") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            OutlinedTextField(
                value = viewModel.inviteCode,
                onValueChange = { viewModel.inviteCode = it },
                label = { Text("Invite code") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            OutlinedTextField(
                value = viewModel.deviceName,
                onValueChange = { viewModel.deviceName = it },
                label = { Text("Device name") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
            )
            viewModel.error?.let {
                Text(it, color = MaterialTheme.colorScheme.error)
            }
            Spacer(Modifier.height(8.dp))
            Button(
                enabled = viewModel.canRegister,
                onClick = { viewModel.register(onRegistered) },
                modifier = Modifier.fillMaxWidth(),
            ) {
                Text(if (viewModel.inflight) "Registering…" else "Register")
            }
        }
    }
}
