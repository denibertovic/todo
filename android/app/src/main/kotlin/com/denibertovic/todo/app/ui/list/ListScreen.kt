package com.denibertovic.todo.app.ui.list

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.FilterList
import androidx.compose.material.icons.filled.RadioButtonUnchecked
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material3.AssistChip
import androidx.compose.material3.Badge
import androidx.compose.material3.BadgedBox
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FloatingActionButton
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import android.widget.Toast
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.displayMetadata
import com.denibertovic.todo.core.types.Metadata

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class, ExperimentalFoundationApi::class)
@Composable
fun ListScreen(
    viewModel: ListViewModel,
    onEditItem: (String) -> Unit,
    onNewItem: () -> Unit,
    onOpenSettings: () -> Unit,
) {
    val items by viewModel.items.collectAsState()
    val context = LocalContext.current

    // Collect one-shot events (toasts)
    LaunchedEffect(Unit) {
        viewModel.events.collect { event ->
            when (event) {
                is ListViewModel.Event.Toast ->
                    Toast.makeText(context, event.message, Toast.LENGTH_SHORT).show()
            }
        }
    }

    // Pre-compute metadata once per item change
    val itemsWithMeta = remember(items) {
        items.map { item ->
            val meta = item.displayMetadata()
            ItemWithMeta(item, meta, metadataSummary(meta))
        }
    }

    val (availableProjects, availableContexts) = remember(itemsWithMeta) {
        val p = sortedSetOf<String>()
        val c = sortedSetOf<String>()
        for (iwm in itemsWithMeta) {
            for (m in iwm.metadata) {
                when (m) {
                    is Metadata.ProjectM -> p.add(m.project.name)
                    is Metadata.ContextM -> c.add(m.context.name)
                    else -> Unit
                }
            }
        }
        p.toList() to c.toList()
    }

    val filtered = remember(
        itemsWithMeta,
        viewModel.selectedProjects.toList(),
        viewModel.selectedContexts.toList(),
    ) {
        val projSet = viewModel.selectedProjects.toSet()
        val ctxSet = viewModel.selectedContexts.toSet()
        itemsWithMeta
            .filter { matchesFilters(it, projSet, ctxSet) }
            .sortedWith(listWithMetaOrdering)
    }

    var showFilterSheet by remember { mutableStateOf(false) }

    BackHandler(enabled = viewModel.inSelectionMode) {
        viewModel.clearSelection()
    }

    if (showFilterSheet) {
        val sheetState = rememberModalBottomSheetState()
        ModalBottomSheet(
            onDismissRequest = { showFilterSheet = false },
            sheetState = sheetState,
        ) {
            FilterSheetContent(
                projects = availableProjects,
                contexts = availableContexts,
                selectedProjects = viewModel.selectedProjects,
                selectedContexts = viewModel.selectedContexts,
                onToggleProject = viewModel::toggleProjectFilter,
                onToggleContext = viewModel::toggleContextFilter,
                onClearAll = viewModel::clearFilters,
            )
        }
    }

    Scaffold(
        topBar = {
            if (viewModel.inSelectionMode) {
                TopAppBar(
                    navigationIcon = {
                        IconButton(onClick = viewModel::clearSelection) {
                            Icon(Icons.Default.Close, contentDescription = "Cancel selection")
                        }
                    },
                    title = { Text("${viewModel.selectedItems.size} selected") },
                    actions = {
                        IconButton(onClick = viewModel::deleteSelected) {
                            Icon(Icons.Default.Delete, contentDescription = "Delete selected")
                        }
                    },
                )
            } else {
                TopAppBar(
                    title = {
                        val label = if (viewModel.activeFilterCount > 0)
                            "Todos (${filtered.size}/${items.size})"
                        else
                            "Todos (${items.size})"
                        Text(label)
                    },
                    actions = {
                        IconButton(onClick = { showFilterSheet = true }) {
                            if (viewModel.activeFilterCount > 0) {
                                BadgedBox(badge = { Badge { Text("${viewModel.activeFilterCount}") } }) {
                                    Icon(Icons.Default.FilterList, contentDescription = "Filters")
                                }
                            } else {
                                Icon(Icons.Default.FilterList, contentDescription = "Filters")
                            }
                        }
                        IconButton(onClick = onOpenSettings) {
                            Icon(Icons.Default.Settings, contentDescription = "Settings")
                        }
                    },
                )
            }
        },
        floatingActionButton = {
            if (!viewModel.inSelectionMode) {
                FloatingActionButton(onClick = onNewItem) {
                    Icon(Icons.Default.Add, contentDescription = "Add todo")
                }
            }
        },
    ) { padding ->
        PullToRefreshBox(
            isRefreshing = viewModel.isRefreshing,
            onRefresh = viewModel::sync,
            modifier = Modifier.padding(padding).fillMaxSize(),
        ) {
            Column(modifier = Modifier.fillMaxSize()) {
                if (viewModel.activeFilterCount > 0 && !viewModel.inSelectionMode) {
                    FlowRow(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = 12.dp, vertical = 4.dp),
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        verticalArrangement = Arrangement.spacedBy(4.dp),
                    ) {
                        viewModel.selectedProjects.toList().forEach { p ->
                            AssistChip(
                                onClick = { viewModel.toggleProjectFilter(p) },
                                label = { Text("+$p") },
                                trailingIcon = {
                                    Icon(Icons.Default.Close, contentDescription = "Remove", modifier = Modifier.padding(0.dp))
                                },
                            )
                        }
                        viewModel.selectedContexts.toList().forEach { c ->
                            AssistChip(
                                onClick = { viewModel.toggleContextFilter(c) },
                                label = { Text("@$c") },
                                trailingIcon = {
                                    Icon(Icons.Default.Close, contentDescription = "Remove", modifier = Modifier.padding(0.dp))
                                },
                            )
                        }
                    }
                }

                LazyColumn(
                    modifier = Modifier.fillMaxSize(),
                    contentPadding = PaddingValues(horizontal = 12.dp, vertical = 8.dp),
                    verticalArrangement = Arrangement.spacedBy(10.dp),
                ) {
                    items(filtered, key = { it.entity.itemId }) { iwm ->
                        val item = iwm.entity
                        if (viewModel.inSelectionMode) {
                            SelectableTodoRow(
                                item = item,
                                metaSummary = iwm.summary,
                                isSelected = item.itemId in viewModel.selectedItems,
                                onToggle = { viewModel.toggleItemSelection(item.itemId) },
                            )
                        } else {
                            SwipeableRow(
                                itemId = item.itemId,
                                isCompleted = item.completed,
                                onComplete = {
                                    viewModel.toggleComplete(item.itemId, item.completed)
                                },
                            ) {
                                TodoRow(
                                    item = item,
                                    metaSummary = iwm.summary,
                                    onTap = { onEditItem(item.itemId) },
                                    onLongPress = { viewModel.startSelection(item.itemId) },
                                )
                            }
                        }
                    }
                }
            }
        }
    }
}

@Composable
private fun SelectableTodoRow(
    item: ItemCacheEntity,
    metaSummary: String,
    isSelected: Boolean,
    onToggle: () -> Unit,
) {
    val containerColor = if (isSelected)
        MaterialTheme.colorScheme.primaryContainer
    else
        MaterialTheme.colorScheme.surfaceContainer

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = containerColor),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .clickable(onClick = onToggle)
                .padding(horizontal = 16.dp, vertical = 14.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Icon(
                imageVector = if (isSelected) Icons.Default.CheckCircle else Icons.Default.RadioButtonUnchecked,
                contentDescription = if (isSelected) "Selected" else "Not selected",
                tint = if (isSelected) MaterialTheme.colorScheme.primary else MaterialTheme.colorScheme.onSurfaceVariant,
                modifier = Modifier.size(24.dp),
            )
            TodoItemContent(
                description = item.description,
                priority = item.priority,
                completed = item.completed,
                metaSummary = metaSummary,
                modifier = Modifier.weight(1f),
            )
        }
    }
}

@OptIn(ExperimentalFoundationApi::class)
@Composable
private fun TodoRow(
    item: ItemCacheEntity,
    metaSummary: String,
    onTap: () -> Unit,
    onLongPress: () -> Unit = {},
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surfaceContainer,
        ),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .combinedClickable(onClick = onTap, onLongClick = onLongPress)
                .padding(horizontal = 16.dp, vertical = 14.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            TodoItemContent(
                description = item.description,
                priority = item.priority,
                completed = item.completed,
                metaSummary = metaSummary,
                modifier = Modifier.fillMaxWidth(),
            )
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun FilterSheetContent(
    projects: List<String>,
    contexts: List<String>,
    selectedProjects: List<String>,
    selectedContexts: List<String>,
    onToggleProject: (String) -> Unit,
    onToggleContext: (String) -> Unit,
    onClearAll: () -> Unit,
) {
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp)
            .padding(bottom = 32.dp),
    ) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text("Filters", style = MaterialTheme.typography.titleLarge)
            if (selectedProjects.isNotEmpty() || selectedContexts.isNotEmpty()) {
                AssistChip(onClick = onClearAll, label = { Text("Clear all") })
            }
        }

        if (projects.isNotEmpty()) {
            Text(
                "Projects",
                style = MaterialTheme.typography.titleSmall,
                modifier = Modifier.padding(top = 16.dp, bottom = 8.dp),
            )
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                projects.forEach { p ->
                    FilterChip(
                        selected = p in selectedProjects,
                        onClick = { onToggleProject(p) },
                        label = { Text("+$p") },
                    )
                }
            }
        }

        if (contexts.isNotEmpty()) {
            if (projects.isNotEmpty()) {
                HorizontalDivider(modifier = Modifier.padding(vertical = 12.dp))
            }
            Text(
                "Contexts",
                style = MaterialTheme.typography.titleSmall,
                modifier = Modifier.padding(bottom = 8.dp),
            )
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                contexts.forEach { c ->
                    FilterChip(
                        selected = c in selectedContexts,
                        onClick = { onToggleContext(c) },
                        label = { Text("@$c") },
                    )
                }
            }
        }
    }
}

// ---------- helpers ----------

internal data class ItemWithMeta(
    val entity: ItemCacheEntity,
    val metadata: List<Metadata>,
    val summary: String,
)

internal fun matchesFilters(
    iwm: ItemWithMeta,
    selectedProjects: Set<String>,
    selectedContexts: Set<String>,
): Boolean {
    if (selectedProjects.isEmpty() && selectedContexts.isEmpty()) return true
    val projects = iwm.metadata.filterIsInstance<Metadata.ProjectM>().map { it.project.name }.toSet()
    val contexts = iwm.metadata.filterIsInstance<Metadata.ContextM>().map { it.context.name }.toSet()
    val projectMatch = selectedProjects.isEmpty() || projects.any { it in selectedProjects }
    val contextMatch = selectedContexts.isEmpty() || contexts.any { it in selectedContexts }
    return projectMatch && contextMatch
}

internal fun metadataSummary(metadata: List<Metadata>): String =
    metadata.joinToString(" ") { m ->
        when (m) {
            is Metadata.ProjectM -> "+${m.project.name}"
            is Metadata.ContextM -> "@${m.context.name}"
            is Metadata.TagM -> m.tag.render()
            is Metadata.Str -> m.value
            is Metadata.LinkM -> m.link.url
        }
    }

internal val listWithMetaOrdering = Comparator<ItemWithMeta> { a, b ->
    val completeCmp = a.entity.completed.compareTo(b.entity.completed)
    if (completeCmp != 0) return@Comparator completeCmp
    val priA = priorityRank(a.entity.priority)
    val priB = priorityRank(b.entity.priority)
    val priCmp = priA.compareTo(priB)
    if (priCmp != 0) return@Comparator priCmp
    b.entity.createdAt.compareTo(a.entity.createdAt)
}

private fun priorityRank(priority: String?): Int =
    if (priority == null) Int.MAX_VALUE else priority[0] - 'A'
