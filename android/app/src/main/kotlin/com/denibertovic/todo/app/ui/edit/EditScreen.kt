package com.denibertovic.todo.app.ui.edit

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
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
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.decodeMetadata
import com.denibertovic.todo.core.parser.TodoParser
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.TodoItem
import com.denibertovic.todo.core.types.renderItem
import kotlinx.coroutines.launch

/**
 * Add / edit screen. Single text field using the same todo.txt
 * syntax the CLI uses — `(A) buy milk +groceries @store due:2026-04-20`.
 * Every keystroke is re-parsed in-memory so we can show a live
 * summary below the input ("Priority: A • 1 project • 1 context •
 * due 2026-04-20") — that's the discoverability hook that lets new
 * users learn the CLI syntax by example.
 *
 * On save: parse, build a [TodoItem], and either emit [OpAdd] for a
 * new item or [OpModifyDescription] + [OpSetPriority] for an edit.
 * We do not try to diff metadata granularly — modifying description
 * or priority after the initial add is enough for v1.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun EditScreen(
    app: TodoApplication,
    itemId: String?,
    onDone: () -> Unit,
) {
    var text by remember { mutableStateOf("") }
    var initialText by remember { mutableStateOf("") }
    val scope = rememberCoroutineScope()

    // Load existing row on first composition.
    LaunchedEffect(itemId) {
        if (itemId != null) {
            val row = app.database.itemCacheDao().getById(itemId) ?: return@LaunchedEffect
            val metadata = row.decodeMetadata()
            val reconstructed = TodoItem(
                priority = row.priority?.let { Priority.fromLetter(it[0]) },
                description = row.description,
                metadata = metadata,
            ).renderItem()
            text = reconstructed
            initialText = reconstructed
        }
    }

    val parsed = remember(text) { TodoParser.parseLine(text).getOrNull() }

    Scaffold(
        topBar = {
            TopAppBar(title = { Text(if (itemId == null) "New todo" else "Edit todo") })
        },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            OutlinedTextField(
                value = text,
                onValueChange = { text = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text("Todo text") },
                placeholder = { Text("(A) buy milk +groceries @store due:2026-04-20") },
                singleLine = false,
            )
            Spacer(Modifier.height(4.dp))
            Text(
                text = parsed?.item?.let { summarize(it) }
                    ?: "Waiting for a valid todo.txt line…",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            Spacer(Modifier.height(4.dp))
            Row(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                modifier = Modifier.fillMaxWidth(),
            ) {
                OutlinedButton(
                    onClick = onDone,
                    modifier = Modifier.weight(1f),
                ) { Text("Cancel") }
                Button(
                    enabled = parsed != null,
                    modifier = Modifier.weight(1f),
                    onClick = {
                        val p = parsed ?: return@Button
                        scope.launch {
                            if (itemId == null) {
                                app.todoActions.add(p.item)
                            } else {
                                val id = ItemId.parse(itemId)
                                app.todoActions.modifyDescription(id, p.item.description)
                                app.todoActions.setPriority(id, p.item.priority)
                            }
                            onDone()
                        }
                    },
                ) { Text("Save") }
            }
        }
    }
}

private fun summarize(item: TodoItem): String {
    val pieces = buildList {
        item.priority?.let { add("Priority: ${it.name}") }
        val projectCount = item.metadata.count { it is com.denibertovic.todo.core.types.Metadata.ProjectM }
        val contextCount = item.metadata.count { it is com.denibertovic.todo.core.types.Metadata.ContextM }
        if (projectCount > 0) add("$projectCount project" + if (projectCount > 1) "s" else "")
        if (contextCount > 0) add("$contextCount context" + if (contextCount > 1) "s" else "")
        val dueTag = item.metadata.firstNotNullOfOrNull { md ->
            (md as? com.denibertovic.todo.core.types.Metadata.TagM)?.tag?.let { tag ->
                when (tag) {
                    is com.denibertovic.todo.core.types.Tag.DueDate -> "due ${tag.date}"
                    is com.denibertovic.todo.core.types.Tag.Next -> "due next"
                    else -> null
                }
            }
        }
        dueTag?.let { add(it) }
    }
    return if (pieces.isEmpty()) "Parsed ✔" else pieces.joinToString(" • ")
}
