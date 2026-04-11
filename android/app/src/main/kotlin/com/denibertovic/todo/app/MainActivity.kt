package com.denibertovic.todo.app

import android.content.Intent
import android.net.Uri
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.foundation.layout.fillMaxSize
import com.denibertovic.todo.app.ui.TodoNavHost

/**
 * Single-activity host. All UI is Compose.
 *
 * Handles the `todo-sync://register?server=...&code=...` deeplink
 * that the CLI's `todo sync invite --qr` command emits — we parse
 * the params out of the intent and pre-fill the onboarding form so
 * the user just has to tap "Register".
 */
class MainActivity : ComponentActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val deeplink = parseRegisterDeeplink(intent)
        setContent {
            MaterialTheme {
                Surface(modifier = Modifier.fillMaxSize()) {
                    var prefilled by remember { mutableStateOf(deeplink) }
                    TodoNavHost(
                        app = application as TodoApplication,
                        prefilledRegistration = prefilled,
                        onPrefillConsumed = { prefilled = null },
                    )
                }
            }
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        // If the app is already running and the user scans a new
        // invite QR, a fresh intent arrives here. We don't handle
        // hot-swapping yet — in practice the user will re-open the
        // onboarding flow from Settings and type the code there.
        setIntent(intent)
    }

    private fun parseRegisterDeeplink(intent: Intent?): PrefilledRegistration? {
        val data: Uri = intent?.data ?: return null
        if (data.scheme != "todo-sync" || data.host != "register") return null
        val server = data.getQueryParameter("server") ?: return null
        val code = data.getQueryParameter("code") ?: return null
        return PrefilledRegistration(serverUrl = server, inviteCode = code)
    }
}

/** Holder for deeplink-sourced onboarding values. */
data class PrefilledRegistration(val serverUrl: String, val inviteCode: String)
