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
    modifier: Modifier = Modifier,
) {
    val color = TodoColors.priority(priority, completed)
    Column(modifier = modifier) {
        val priorityLabel = priority?.let { "($it) " } ?: ""
        Text(
            text = priorityLabel + description,
            color = color,
            fontWeight = if (priority != null) FontWeight.SemiBold else FontWeight.Normal,
            textDecoration = if (completed) TextDecoration.LineThrough else TextDecoration.None,
        )
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
