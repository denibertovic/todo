package com.denibertovic.todo.app.ui.onboarding

import android.os.Build
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.denibertovic.todo.app.PrefilledRegistration
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.RegistrationRepository
import kotlinx.coroutines.launch

class OnboardingViewModel(
    private val registrationRepository: RegistrationRepository,
    prefilled: PrefilledRegistration?,
) : ViewModel() {

    var serverUrl by mutableStateOf(prefilled?.serverUrl ?: "https://")
    var inviteCode by mutableStateOf(prefilled?.inviteCode ?: "")
    var deviceName by mutableStateOf(defaultDeviceName())
    var inflight by mutableStateOf(false)
        private set
    var error by mutableStateOf<String?>(null)
        private set
    var showScanner by mutableStateOf(false)

    val canRegister: Boolean
        get() = !inflight && serverUrl.startsWith("http") && inviteCode.isNotBlank()

    fun register(onSuccess: () -> Unit) {
        inflight = true
        error = null
        viewModelScope.launch {
            try {
                val result = registrationRepository.register(
                    serverUrl = serverUrl.trim().trimEnd('/'),
                    inviteCode = inviteCode.trim(),
                    deviceName = deviceName.ifBlank { defaultDeviceName() },
                )
                when (result) {
                    is RegistrationRepository.Result.Success -> onSuccess()
                    is RegistrationRepository.Result.Failure ->
                        error = "Registration failed: ${result.error.message}"
                }
            } catch (e: Exception) {
                error = "Registration failed: ${e.message}"
            } finally {
                inflight = false
            }
        }
    }

    fun onScanResult(result: ScannedRegistration) {
        showScanner = false
        result.serverUrl?.takeIf { it.isNotBlank() }?.let { serverUrl = it }
        result.inviteCode?.takeIf { it.isNotBlank() }?.let { inviteCode = it }
        if (result.serverUrl == null && result.inviteCode == null) {
            error = "Scanned QR was not a recognized invite code. Got: ${result.rawText.take(80)}"
        } else {
            error = null
        }
    }

    class Factory(
        private val app: TodoApplication,
        private val prefilled: PrefilledRegistration?,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            OnboardingViewModel(app.registrationRepository, prefilled) as T
    }
}

internal fun defaultDeviceName(): String {
    val model = Build.MODEL ?: "android"
    val manufacturer = Build.MANUFACTURER ?: ""
    return if (manufacturer.isBlank() || model.startsWith(manufacturer, ignoreCase = true)) {
        model
    } else {
        "$manufacturer $model"
    }
}
