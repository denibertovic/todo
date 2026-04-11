package com.denibertovic.todo.app.ui

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.navigation.NavHostController
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import com.denibertovic.todo.app.PrefilledRegistration
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.ui.edit.EditScreen
import com.denibertovic.todo.app.ui.list.ListScreen
import com.denibertovic.todo.app.ui.onboarding.OnboardingScreen
import com.denibertovic.todo.app.ui.settings.SettingsScreen
import kotlinx.coroutines.launch

/**
 * Destination names used across the app. Kept as top-level constants
 * rather than an enum because route strings can carry path arguments
 * (`edit/{itemId}`) and enums don't compose well with that.
 */
object Routes {
    const val ONBOARDING = "onboarding"
    const val LIST = "list"
    const val EDIT_NEW = "edit/new"
    const val EDIT = "edit/{itemId}"
    const val SETTINGS = "settings"

    fun editExisting(itemId: String) = "edit/$itemId"
}

@Composable
fun TodoNavHost(
    app: TodoApplication,
    prefilledRegistration: PrefilledRegistration?,
    onPrefillConsumed: () -> Unit,
) {
    val nav = rememberNavController()
    val scope = rememberCoroutineScope()

    // First-launch gating: if no device has been registered yet,
    // force-route into onboarding. After a successful registration
    // the start destination flips to `list` so we don't bounce the
    // user back through onboarding on every cold start.
    var startDestination by remember { mutableStateOf<String?>(null) }
    LaunchedEffect(Unit) {
        startDestination = if (app.registrationRepository.isRegistered()) Routes.LIST else Routes.ONBOARDING
    }
    val start = startDestination ?: return

    NavHost(
        navController = nav,
        startDestination = start,
    ) {
        composable(Routes.ONBOARDING) {
            OnboardingScreen(
                app = app,
                prefilled = prefilledRegistration,
                onPrefillConsumed = onPrefillConsumed,
                onRegistered = {
                    nav.navigate(Routes.LIST) {
                        popUpTo(Routes.ONBOARDING) { inclusive = true }
                    }
                },
            )
        }

        composable(Routes.LIST) {
            ListScreen(
                app = app,
                onEditItem = { itemId -> nav.navigate(Routes.editExisting(itemId)) },
                onNewItem = { nav.navigate(Routes.EDIT_NEW) },
                onOpenSettings = { nav.navigate(Routes.SETTINGS) },
            )
        }

        composable(Routes.EDIT_NEW) {
            EditScreen(
                app = app,
                itemId = null,
                onDone = { nav.popBackStack() },
            )
        }

        composable(Routes.EDIT) { backStack ->
            val itemId = backStack.arguments?.getString("itemId")
            EditScreen(
                app = app,
                itemId = itemId,
                onDone = { nav.popBackStack() },
            )
        }

        composable(Routes.SETTINGS) {
            SettingsScreen(
                app = app,
                onBack = { nav.popBackStack() },
                onWiped = {
                    nav.navigate(Routes.ONBOARDING) {
                        popUpTo(Routes.LIST) { inclusive = true }
                    }
                },
            )
        }
    }
}
