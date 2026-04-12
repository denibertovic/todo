package com.denibertovic.todo.app.ui

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.lifecycle.viewmodel.compose.viewModel
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import com.denibertovic.todo.app.PrefilledRegistration
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.ui.edit.EditScreen
import com.denibertovic.todo.app.ui.edit.EditViewModel
import com.denibertovic.todo.app.ui.list.ListScreen
import com.denibertovic.todo.app.ui.list.ListViewModel
import com.denibertovic.todo.app.ui.onboarding.OnboardingScreen
import com.denibertovic.todo.app.ui.onboarding.OnboardingViewModel
import com.denibertovic.todo.app.ui.settings.SettingsScreen
import com.denibertovic.todo.app.ui.settings.SettingsViewModel

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

    var startDestination by remember { mutableStateOf<String?>(null) }
    LaunchedEffect(Unit) {
        startDestination = if (app.registrationRepository.isRegistered()) Routes.LIST else Routes.ONBOARDING
    }
    val start = startDestination ?: return

    NavHost(navController = nav, startDestination = start) {
        composable(Routes.ONBOARDING) {
            val vm: OnboardingViewModel = viewModel(
                factory = OnboardingViewModel.Factory(app, prefilledRegistration),
            )
            LaunchedEffect(prefilledRegistration) {
                if (prefilledRegistration != null) onPrefillConsumed()
            }
            OnboardingScreen(
                viewModel = vm,
                onRegistered = {
                    nav.navigate(Routes.LIST) {
                        popUpTo(Routes.ONBOARDING) { inclusive = true }
                    }
                },
            )
        }

        composable(Routes.LIST) {
            val vm: ListViewModel = viewModel(factory = ListViewModel.Factory(app))
            ListScreen(
                viewModel = vm,
                onEditItem = { itemId -> nav.navigate(Routes.editExisting(itemId)) },
                onNewItem = { nav.navigate(Routes.EDIT_NEW) },
                onOpenSettings = { nav.navigate(Routes.SETTINGS) },
            )
        }

        composable(Routes.EDIT_NEW) {
            val vm: EditViewModel = viewModel(factory = EditViewModel.Factory(app, null))
            EditScreen(viewModel = vm, onDone = { nav.popBackStack() })
        }

        composable(Routes.EDIT) { backStack ->
            val itemId = backStack.arguments?.getString("itemId")
            val vm: EditViewModel = viewModel(factory = EditViewModel.Factory(app, itemId))
            EditScreen(viewModel = vm, onDone = { nav.popBackStack() })
        }

        composable(Routes.SETTINGS) {
            val vm: SettingsViewModel = viewModel(factory = SettingsViewModel.Factory(app))
            SettingsScreen(
                viewModel = vm,
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
