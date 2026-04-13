package com.denibertovic.todo.app.ui.list

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.ui.theme.TodoColors

/**
 * Shared content rendering for a single todo item. Used by both the
 * normal swipeable row and the multi-select row, eliminating the
 * duplicate text/color/decoration logic that lived in both places.
 */
@Composable
fun TodoItemContent(
    description: String,
    priority: String?,
    completed: Boolean,
    metaSummary: String,
    isUrgent: Boolean = false,
    isOverdue: Boolean = false,
    dueLabel: String? = null,
    modifier: Modifier = Modifier,
) {
    val color = TodoColors.priority(priority, completed)
    Column(modifier = modifier) {
        val priorityLabel = priority?.let { "($it) " } ?: ""
        val decoration = when {
            completed -> TextDecoration.LineThrough
            isUrgent -> TextDecoration.Underline
            else -> TextDecoration.None
        }
        Text(
            text = priorityLabel + description,
            color = color,
            fontWeight = if (isUrgent || priority != null) FontWeight.Bold else FontWeight.Normal,
            textDecoration = decoration,
        )
        if (dueLabel != null) {
            val dueColor = when {
                completed -> MaterialTheme.colorScheme.onSurfaceVariant
                isOverdue -> TodoColors.overdue()
                isUrgent -> TodoColors.dueToday()
                else -> MaterialTheme.colorScheme.onSurfaceVariant
            }
            Text(
                text = dueLabel,
                style = MaterialTheme.typography.bodySmall,
                color = dueColor,
                fontWeight = if (isUrgent && !completed) FontWeight.SemiBold else FontWeight.Normal,
                modifier = Modifier.padding(top = 2.dp),
            )
        }
        if (metaSummary.isNotEmpty()) {
            Text(
                text = metaSummary,
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                modifier = Modifier.padding(top = 4.dp),
            )
        }
    }
}
