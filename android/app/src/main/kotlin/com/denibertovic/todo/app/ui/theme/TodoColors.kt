package com.denibertovic.todo.app.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color

/**
 * Semantic color helpers that adapt to light/dark theme. Priority
 * colors and swipe affordance colors don't map cleanly to Material3
 * roles, so we provide light/dark variants manually and lean on
 * [MaterialTheme.colorScheme] where a semantic role exists.
 */
object TodoColors {

    @Composable
    fun priority(priority: String?, completed: Boolean): Color {
        if (completed) return MaterialTheme.colorScheme.outline
        val isDark = isSystemInDarkTheme()
        return when (priority) {
            "A" -> MaterialTheme.colorScheme.error
            "B" -> if (isDark) Color(0xFFFFD54F) else Color(0xFFF9A825)
            "C" -> if (isDark) Color(0xFF81C784) else Color(0xFF43A047)
            null -> Color.Unspecified
            else -> if (isDark) Color(0xFF4DD0E1) else Color(0xFF00ACC1)
        }
    }

    @Composable
    fun overdue(): Color = MaterialTheme.colorScheme.error

    @Composable
    fun dueToday(): Color =
        if (isSystemInDarkTheme()) Color(0xFFFFB74D) else Color(0xFFFB8C00)

    @Composable
    fun swipeComplete(): Color =
        if (isSystemInDarkTheme()) Color(0xFF66BB6A) else Color(0xFF43A047)

    @Composable
    fun swipeUncomplete(): Color =
        if (isSystemInDarkTheme()) Color(0xFFFFB74D) else Color(0xFFFB8C00)
}
