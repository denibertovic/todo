package com.denibertovic.todo.app.ui.onboarding

import android.Manifest
import android.content.pm.PackageManager
import android.net.Uri
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.camera.core.CameraSelector
import androidx.camera.core.ImageAnalysis
import androidx.camera.core.ImageProxy
import androidx.camera.core.Preview
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.lifecycle.compose.LocalLifecycleOwner
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import com.google.mlkit.vision.barcode.BarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import com.google.mlkit.vision.common.InputImage
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
 * A "value" holder representing anything useful the scanner might
 * extract from a QR code. We try to interpret the decoded text as a
 * `todo-sync://register?server=...&code=...` deeplink; failing that
 * we just return the raw text so the caller can decide whether to
 * treat it as a bare invite code.
 */
data class ScannedRegistration(
    val serverUrl: String?,
    val inviteCode: String?,
    val rawText: String,
)

/**
 * Full-screen CameraX preview with ML Kit barcode scanning wired up.
 * As soon as a QR frame decodes to something non-empty [onScanned]
 * is invoked and the scanner pauses — repeated triggers while the
 * user is still on this screen would cause flicker and duplicate
 * navigation.
 *
 * Camera permission is requested lazily on first entry. If the user
 * denies, we render a static explanation with a "Grant permission"
 * button and surface a [onCancel] to bounce back to the form.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun QrScannerScreen(
    onScanned: (ScannedRegistration) -> Unit,
    onCancel: () -> Unit,
) {
    val context = LocalContext.current
    var hasPermission by remember {
        mutableStateOf(
            ContextCompat.checkSelfPermission(
                context,
                Manifest.permission.CAMERA,
            ) == PackageManager.PERMISSION_GRANTED,
        )
    }

    val permissionLauncher = rememberLauncherForActivityResult(
        contract = ActivityResultContracts.RequestPermission(),
    ) { granted -> hasPermission = granted }

    LaunchedEffect(Unit) {
        if (!hasPermission) {
            permissionLauncher.launch(Manifest.permission.CAMERA)
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Scan invite QR") },
                navigationIcon = {
                    IconButton(onClick = onCancel) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Back")
                    }
                },
            )
        },
    ) { padding ->
        Box(modifier = Modifier.fillMaxSize().padding(padding)) {
            if (hasPermission) {
                CameraPreview(onScanned = onScanned)
                // Small overlay with instructions. Pointers for the
                // user: what we're looking for and how to fall back.
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp),
                    verticalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    Text(
                        text = "Point the camera at the QR code printed by `todo-sync-server generate-invite-code --qr`.",
                        color = MaterialTheme.colorScheme.onSurface,
                    )
                }
            } else {
                PermissionDeniedFallback(
                    onRequest = { permissionLauncher.launch(Manifest.permission.CAMERA) },
                    onCancel = onCancel,
                )
            }
        }
    }
}

@Composable
private fun PermissionDeniedFallback(
    onRequest: () -> Unit,
    onCancel: () -> Unit,
) {
    Column(
        modifier = Modifier.fillMaxSize().padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        Text("Camera permission is required to scan the invite QR code.")
        Text(
            text = "You can also type the server URL and invite code manually on the previous screen.",
            style = MaterialTheme.typography.bodySmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Spacer(Modifier.height(8.dp))
        Button(onClick = onRequest, modifier = Modifier.fillMaxWidth()) {
            Text("Grant permission")
        }
        Button(onClick = onCancel, modifier = Modifier.fillMaxWidth()) {
            Text("Back to manual entry")
        }
    }
}

/**
 * CameraX PreviewView + ImageAnalysis pipeline. ML Kit's barcode
 * scanner consumes `ImageProxy` frames and fires a one-shot
 * callback on the first successful decode. We debounce via an
 * `AtomicBoolean` sentinel so multi-frame matches don't invoke the
 * callback repeatedly.
 */
@Composable
private fun CameraPreview(
    onScanned: (ScannedRegistration) -> Unit,
) {
    val context = LocalContext.current
    val lifecycleOwner = LocalLifecycleOwner.current
    val executor = remember { Executors.newSingleThreadExecutor() }
    val previewView = remember {
        PreviewView(context).apply {
            scaleType = PreviewView.ScaleType.FILL_CENTER
        }
    }
    val scanned = remember { java.util.concurrent.atomic.AtomicBoolean(false) }

    DisposableEffect(Unit) {
        val scanner = BarcodeScanning.getClient()
        val providerFuture = ProcessCameraProvider.getInstance(context)
        providerFuture.addListener({
            val provider = providerFuture.get()
            val preview = Preview.Builder().build().also {
                it.setSurfaceProvider(previewView.surfaceProvider)
            }
            val analysis = ImageAnalysis.Builder()
                .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                .build()
            analysis.setAnalyzer(executor) { proxy ->
                processFrame(proxy, scanner, scanned, onScanned)
            }
            try {
                provider.unbindAll()
                provider.bindToLifecycle(
                    lifecycleOwner,
                    CameraSelector.DEFAULT_BACK_CAMERA,
                    preview,
                    analysis,
                )
            } catch (_: Exception) {
                // Swallow: the user cancelled or the device lacks a
                // back camera. The Scaffold stays visible either way.
            }
        }, ContextCompat.getMainExecutor(context))

        onDispose {
            providerFuture.get()?.unbindAll()
            executor.shutdown()
            scanner.close()
        }
    }

    AndroidView(
        factory = { previewView },
        modifier = Modifier.fillMaxSize(),
    )
}

@androidx.annotation.OptIn(androidx.camera.core.ExperimentalGetImage::class)
private fun processFrame(
    proxy: ImageProxy,
    scanner: com.google.mlkit.vision.barcode.BarcodeScanner,
    sentinel: java.util.concurrent.atomic.AtomicBoolean,
    onScanned: (ScannedRegistration) -> Unit,
) {
    if (sentinel.get()) {
        proxy.close()
        return
    }
    val media = proxy.image
    if (media == null) {
        proxy.close()
        return
    }
    val input = InputImage.fromMediaImage(media, proxy.imageInfo.rotationDegrees)
    scanner.process(input)
        .addOnSuccessListener { barcodes ->
            val raw = barcodes
                .firstOrNull { it.valueType != Barcode.TYPE_UNKNOWN || it.rawValue != null }
                ?.rawValue
            if (!raw.isNullOrBlank() && sentinel.compareAndSet(false, true)) {
                onScanned(parseScanResult(raw))
            }
        }
        .addOnCompleteListener { proxy.close() }
}

/**
 * Interpret scanned text. Accepted forms:
 *
 * - `todo-sync://register?server=<urlencoded>&code=<uuid>` — the
 *   deeplink emitted by `todo-sync-server generate-invite-code --qr`
 * - A bare invite code UUID — useful when the user pre-formats the
 *   QR themselves or encodes something shorter. The caller is
 *   expected to fall back to a manual server URL in that case.
 * - Anything else — returned as [rawText] so the caller can decide
 *   how to surface it (usually as an inline error).
 */
internal fun parseScanResult(text: String): ScannedRegistration {
    val trimmed = text.trim()
    // Case 1: deeplink
    if (trimmed.startsWith("todo-sync://")) {
        val uri = runCatching { Uri.parse(trimmed) }.getOrNull()
        if (uri != null && uri.host == "register") {
            return ScannedRegistration(
                serverUrl = uri.getQueryParameter("server"),
                inviteCode = uri.getQueryParameter("code"),
                rawText = trimmed,
            )
        }
    }
    // Case 2: bare UUID
    if (trimmed.matches(uuidRegex)) {
        return ScannedRegistration(
            serverUrl = null,
            inviteCode = trimmed,
            rawText = trimmed,
        )
    }
    // Case 3: something else — let the caller surface it
    return ScannedRegistration(serverUrl = null, inviteCode = null, rawText = trimmed)
}

private val uuidRegex = Regex(
    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
)
