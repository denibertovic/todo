package com.denibertovic.todo.app.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.horizontalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material3.Checkbox
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FloatingActionButton
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.decodeMetadata
import com.denibertovic.todo.core.types.Context
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Project
import kotlinx.coroutines.launch

/**
 * Main list screen. Shows every non-deleted item from the cache,
 * sorted by priority then creation date. Filter chips derived from
 * the active project and context set let the user narrow down to a
 * specific subset — matches the CLI's `todo ls +project @context`
 * filter semantics.
 *
 * Actions are deliberately minimal for v1:
 * - Tap to edit
 * - Checkbox to complete/uncomplete
 * - FAB for add
 * - Overflow for Sync now / Archive completed / Settings
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ListScreen(
    app: TodoApplication,
    onEditItem: (String) -> Unit,
    onNewItem: () -> Unit,
    onOpenSettings: () -> Unit,
) {
    val items by app.database.itemCacheDao().observeVisible().collectAsState(initial = emptyList())
    val scope = rememberCoroutineScope()
    var isRefreshing by remember { mutableStateOf(false) }
    val snackbarHostState = remember { SnackbarHostState() }

    // Extract unique projects and contexts across the current item
    // set. These are the pool from which filter chips are drawn.
    val (projects, contexts) = remember(items) { projectsAndContexts(items) }

    // Local filter state. Multi-select: AND between categories, OR
    // within a category — matching the CLI's `hasProjAndCtx`
    // semantics in `src/Todo/Lib.hs:159-167`.
    // SnapshotStateList so Compose recomposes on add/remove.
    val selectedProjects = remember { mutableStateListOf<String>() }
    val selectedContexts = remember { mutableStateListOf<String>() }

    val filtered = remember(items, selectedProjects.toList(), selectedContexts.toList()) {
        items.filter { matchesFilters(it, selectedProjects.toSet(), selectedContexts.toSet()) }
            .sortedWith(listOrdering)
    }

    Scaffold(
        snackbarHost = { SnackbarHost(snackbarHostState) },
        topBar = {
            TopAppBar(
                title = {
                    val hasFilters = selectedProjects.isNotEmpty() || selectedContexts.isNotEmpty()
                    val label = if (hasFilters) "Todos (${filtered.size}/${items.size})" else "Todos (${items.size})"
                    Text(label)
                },
                actions = {
                    IconButton(onClick = onOpenSettings) {
                        Icon(Icons.Default.Settings, contentDescription = "Settings")
                    }
                },
            )
        },
        floatingActionButton = {
            FloatingActionButton(onClick = onNewItem) {
                Icon(Icons.Default.Add, contentDescription = "Add todo")
            }
        },
    ) { padding ->
        PullToRefreshBox(
            isRefreshing = isRefreshing,
            onRefresh = {
                isRefreshing = true
                scope.launch {
                    val result = app.syncRepository.sync()
                    isRefreshing = false
                    val message = when (result) {
                        is SyncRepository.Result.Success ->
                            "Synced: ${result.sent} sent, ${result.received} received"
                        is SyncRepository.Result.NotRegistered ->
                            "Not registered with sync server"
                        is SyncRepository.Result.Failure ->
                            "Sync failed: ${result.error.message}"
                    }
                    snackbarHostState.showSnackbar(message)
                }
            },
            modifier = Modifier.padding(padding).fillMaxSize(),
        ) {
            Column(modifier = Modifier.fillMaxSize()) {
                // Filter chips. Re-render when the item set changes so
                // new projects/contexts become available immediately.
                if (projects.isNotEmpty() || contexts.isNotEmpty()) {
                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .horizontalScroll(rememberScrollState())
                            .padding(horizontal = 12.dp, vertical = 8.dp),
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                    ) {
                        projects.forEach { p ->
                            val selected = p in selectedProjects
                            FilterChip(
                                selected = selected,
                                onClick = {
                                    if (selected) selectedProjects.remove(p)
                                    else selectedProjects.add(p)
                                },
                                label = { Text("+$p") },
                            )
                        }
                        contexts.forEach { c ->
                            val selected = c in selectedContexts
                            FilterChip(
                                selected = selected,
                                onClick = {
                                    if (selected) selectedContexts.remove(c)
                                    else selectedContexts.add(c)
                                },
                                label = { Text("@$c") },
                            )
                        }
                    }
                }

                LazyColumn(modifier = Modifier.fillMaxSize()) {
                    items(filtered, key = { it.itemId }) { item ->
                        TodoRow(
                            item = item,
                            onToggleComplete = { completed ->
                                scope.launch {
                                    val id = ItemId.parse(item.itemId)
                                    if (completed) app.todoActions.complete(id)
                                    else app.todoActions.uncomplete(id)
                                }
                            },
                            onTap = { onEditItem(item.itemId) },
                        )
                    }
                }
            }
        }
    }
}

@Composable
private fun TodoRow(
    item: ItemCacheEntity,
    onToggleComplete: (Boolean) -> Unit,
    onTap: () -> Unit,
) {
    val color = priorityColor(item.priority, item.completed)
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onTap)
            .padding(horizontal = 12.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Checkbox(
            checked = item.completed,
            onCheckedChange = onToggleComplete,
        )
        Column(modifier = Modifier.fillMaxWidth()) {
            val priorityLabel = item.priority?.let { "($it) " } ?: ""
            Text(
                text = priorityLabel + item.description,
                color = color,
                fontWeight = if (item.priority != null) FontWeight.SemiBold else FontWeight.Normal,
                textDecoration = if (item.completed) TextDecoration.LineThrough else TextDecoration.None,
            )
            val metadata = item.decodeMetadata()
            if (metadata.isNotEmpty()) {
                Text(
                    text = metadataSummary(metadata),
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }
    }
}

private fun projectsAndContexts(items: List<ItemCacheEntity>): Pair<List<String>, List<String>> {
    val projects = sortedSetOf<String>()
    val contexts = sortedSetOf<String>()
    for (item in items) {
        for (m in item.decodeMetadata()) {
            when (m) {
                is Metadata.ProjectM -> projects.add(m.project.name)
                is Metadata.ContextM -> contexts.add(m.context.name)
                else -> Unit
            }
        }
    }
    return projects.toList() to contexts.toList()
}

private fun matchesFilters(
    item: ItemCacheEntity,
    selectedProjects: Set<String>,
    selectedContexts: Set<String>,
): Boolean {
    if (selectedProjects.isEmpty() && selectedContexts.isEmpty()) return true
    val metadata = item.decodeMetadata()
    val projects = metadata.filterIsInstance<Metadata.ProjectM>().map { it.project.name }.toSet()
    val contexts = metadata.filterIsInstance<Metadata.ContextM>().map { it.context.name }.toSet()
    val projectMatch = selectedProjects.isEmpty() || projects.any { it in selectedProjects }
    val contextMatch = selectedContexts.isEmpty() || contexts.any { it in selectedContexts }
    return projectMatch && contextMatch
}

private fun metadataSummary(metadata: List<Metadata>): String =
    metadata.joinToString(" ") { m ->
        when (m) {
            is Metadata.ProjectM -> "+${m.project.name}"
            is Metadata.ContextM -> "@${m.context.name}"
            is Metadata.TagM -> m.tag.render()
            is Metadata.Str -> m.value
            is Metadata.LinkM -> m.link.url
        }
    }

/**
 * Priority color palette matching the CLI's ANSI colors at
 * `src/Todo/Lib.hs:148-155`: A=red, B=yellow, C=green, others=cyan,
 * completed items go grey regardless of priority.
 */
private fun priorityColor(priority: String?, completed: Boolean): Color {
    if (completed) return Color(0xFF888888)
    return when (priority) {
        "A" -> Color(0xFFE53935)
        "B" -> Color(0xFFF9A825)
        "C" -> Color(0xFF43A047)
        null -> Color.Unspecified
        else -> Color(0xFF00ACC1)
    }
}

/**
 * Sort order: incomplete-first, then priority A..Z (no-priority last),
 * then newest first. Matches the CLI's list ordering so the phone
 * doesn't look weird next to a terminal showing the same todos.
 */
private val listOrdering = Comparator<ItemCacheEntity> { a, b ->
    val completeCmp = a.completed.compareTo(b.completed)
    if (completeCmp != 0) return@Comparator completeCmp
    val priA = priorityRank(a.priority)
    val priB = priorityRank(b.priority)
    val priCmp = priA.compareTo(priB)
    if (priCmp != 0) return@Comparator priCmp
    b.createdAt.compareTo(a.createdAt)
}

private fun priorityRank(priority: String?): Int =
    if (priority == null) Int.MAX_VALUE else priority[0] - 'A'
