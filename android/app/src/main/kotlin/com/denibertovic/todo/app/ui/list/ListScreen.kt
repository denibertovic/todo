package com.denibertovic.todo.app.ui.list

import androidx.activity.compose.BackHandler
import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.Done
import androidx.compose.material.icons.filled.FilterList
import androidx.compose.material.icons.filled.RadioButtonUnchecked
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material.icons.filled.Undo
import androidx.compose.material3.AssistChip
import androidx.compose.material3.Badge
import androidx.compose.material3.BadgedBox
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FloatingActionButton
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SnackbarDuration
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.SnackbarResult
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onSizeChanged
import android.view.HapticFeedbackConstants
import androidx.compose.ui.platform.LocalHapticFeedback
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.decodeMetadata
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Metadata
import kotlinx.coroutines.launch

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class, ExperimentalFoundationApi::class)
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

    val (projects, contexts) = remember(items) { projectsAndContexts(items) }

    val selectedProjects = remember { mutableStateListOf<String>() }
    val selectedContexts = remember { mutableStateListOf<String>() }
    val activeFilterCount = selectedProjects.size + selectedContexts.size

    var showFilterSheet by remember { mutableStateOf(false) }

    // Multi-select state
    val selectedItems = remember { mutableStateListOf<String>() }
    val inSelectionMode = selectedItems.isNotEmpty()

    // Back press exits selection mode
    BackHandler(enabled = inSelectionMode) {
        selectedItems.clear()
    }

    val filtered = remember(items, selectedProjects.toList(), selectedContexts.toList()) {
        items.filter { matchesFilters(it, selectedProjects.toSet(), selectedContexts.toSet()) }
            .sortedWith(listOrdering)
    }

    if (showFilterSheet) {
        val sheetState = rememberModalBottomSheetState()
        ModalBottomSheet(
            onDismissRequest = { showFilterSheet = false },
            sheetState = sheetState,
        ) {
            FilterSheetContent(
                projects = projects,
                contexts = contexts,
                selectedProjects = selectedProjects,
                selectedContexts = selectedContexts,
                onClearAll = {
                    selectedProjects.clear()
                    selectedContexts.clear()
                },
            )
        }
    }

    Scaffold(
        snackbarHost = { SnackbarHost(snackbarHostState) },
        topBar = {
            if (inSelectionMode) {
                // Selection mode top bar
                TopAppBar(
                    navigationIcon = {
                        IconButton(onClick = { selectedItems.clear() }) {
                            Icon(Icons.Default.Close, contentDescription = "Cancel selection")
                        }
                    },
                    title = { Text("${selectedItems.size} selected") },
                    actions = {
                        IconButton(onClick = {
                            val toDelete = selectedItems.toList()
                            selectedItems.clear()
                            scope.launch {
                                for (id in toDelete) {
                                    app.todoActions.delete(ItemId.parse(id))
                                }
                                snackbarHostState.showSnackbar(
                                    message = "Deleted ${toDelete.size} items",
                                    duration = SnackbarDuration.Short,
                                )
                            }
                        }) {
                            Icon(Icons.Default.Delete, contentDescription = "Delete selected")
                        }
                    },
                )
            } else {
                // Normal top bar
                TopAppBar(
                    title = {
                        val label = if (activeFilterCount > 0)
                            "Todos (${filtered.size}/${items.size})"
                        else
                            "Todos (${items.size})"
                        Text(label)
                    },
                    actions = {
                        IconButton(onClick = { showFilterSheet = true }) {
                            if (activeFilterCount > 0) {
                                BadgedBox(badge = { Badge { Text("$activeFilterCount") } }) {
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
            if (!inSelectionMode) {
                FloatingActionButton(onClick = onNewItem) {
                    Icon(Icons.Default.Add, contentDescription = "Add todo")
                }
            }
        },
    ) { padding ->
        PullToRefreshBox(
            isRefreshing = isRefreshing,
            onRefresh = {
                if (!inSelectionMode) {
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
                }
            },
            modifier = Modifier.padding(padding).fillMaxSize(),
        ) {
            Column(modifier = Modifier.fillMaxSize()) {
                if (activeFilterCount > 0 && !inSelectionMode) {
                    FlowRow(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = 12.dp, vertical = 4.dp),
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        verticalArrangement = Arrangement.spacedBy(4.dp),
                    ) {
                        selectedProjects.toList().forEach { p ->
                            AssistChip(
                                onClick = { selectedProjects.remove(p) },
                                label = { Text("+$p") },
                                trailingIcon = {
                                    Icon(Icons.Default.Close, contentDescription = "Remove", modifier = Modifier.padding(0.dp))
                                },
                            )
                        }
                        selectedContexts.toList().forEach { c ->
                            AssistChip(
                                onClick = { selectedContexts.remove(c) },
                                label = { Text("@$c") },
                                trailingIcon = {
                                    Icon(Icons.Default.Close, contentDescription = "Remove", modifier = Modifier.padding(0.dp))
                                },
                            )
                        }
                    }
                }

                LazyColumn(modifier = Modifier.fillMaxSize()) {
                    items(filtered, key = { it.itemId }) { item ->
                        if (inSelectionMode) {
                            SelectableTodoRow(
                                item = item,
                                isSelected = item.itemId in selectedItems,
                                onToggle = {
                                    if (item.itemId in selectedItems) selectedItems.remove(item.itemId)
                                    else selectedItems.add(item.itemId)
                                },
                            )
                        } else {
                            SwipeableTodoRow(
                                item = item,
                                onComplete = {
                                    scope.launch {
                                        val id = ItemId.parse(item.itemId)
                                        if (item.completed) {
                                            app.todoActions.uncomplete(id)
                                            snackbarHostState.showSnackbar(
                                                message = "Marked incomplete",
                                                duration = SnackbarDuration.Short,
                                            )
                                        } else {
                                            app.todoActions.complete(id)
                                            val result = snackbarHostState.showSnackbar(
                                                message = "Completed",
                                                actionLabel = "Undo",
                                                duration = SnackbarDuration.Short,
                                            )
                                            if (result == SnackbarResult.ActionPerformed) {
                                                app.todoActions.uncomplete(id)
                                            }
                                        }
                                    }
                                },
                                onTap = { onEditItem(item.itemId) },
                                onLongPress = { selectedItems.add(item.itemId) },
                            )
                        }
                        HorizontalDivider(
                            modifier = Modifier.padding(horizontal = 16.dp),
                            color = MaterialTheme.colorScheme.outlineVariant,
                        )
                    }
                }
            }
        }
    }
}

/**
 * Todoist-style edge-gated swipe to complete. The gesture is only
 * recognized if the finger starts within the first 48dp of the row
 * (the left edge zone). Dragging past 35% of the row width commits
 * the action with haptic feedback. The row animates back to rest
 * after triggering.
 */
@Composable
private fun SwipeableTodoRow(
    item: ItemCacheEntity,
    onComplete: () -> Unit,
    onTap: () -> Unit,
    onLongPress: () -> Unit,
) {
    val scope = rememberCoroutineScope()
    val view = LocalView.current
    val edgeZoneFraction = 0.4f // swipe must start in the left 40% of the row
    val commitFraction = 0.35f

    val offsetX = remember { Animatable(0f) }
    var rowWidthPx by remember { mutableStateOf(1f) }
    var hasPassedThreshold by remember { mutableStateOf(false) }
    var isEdgeSwipe by remember { mutableStateOf(false) }

    val progress = (offsetX.value / rowWidthPx).coerceIn(0f, 1f)
    val committed = progress >= commitFraction

    // Green for complete, orange for uncomplete
    val swipeColor = if (item.completed) Color(0xFFFB8C00) else Color(0xFF43A047)
    val swipeIcon = if (item.completed) Icons.Default.Undo else Icons.Default.Done
    val bgColor = swipeColor.copy(alpha = progress.coerceIn(0f, 1f))
    val iconAlpha = ((progress / commitFraction) * 1.5f).coerceIn(0f, 1f)

    Box(
        modifier = Modifier
            .fillMaxWidth()
            .onSizeChanged { rowWidthPx = it.width.toFloat().coerceAtLeast(1f) }
    ) {
        // Background layer
        Box(
            modifier = Modifier
                .matchParentSize()
                .background(bgColor)
                .padding(start = 20.dp),
            contentAlignment = Alignment.CenterStart,
        ) {
            Icon(
                swipeIcon,
                contentDescription = null,
                tint = Color.White,
                modifier = Modifier.alpha(iconAlpha),
            )
        }

        // Foreground row
        Box(
            modifier = Modifier
                .offset { IntOffset(offsetX.value.toInt(), 0) }
                .pointerInput(item.itemId, item.completed) {
                    detectHorizontalDragGestures(
                        onDragStart = { startOffset ->
                            isEdgeSwipe = startOffset.x <= rowWidthPx * edgeZoneFraction
                            hasPassedThreshold = false
                        },
                        onDragEnd = {
                            if (isEdgeSwipe && offsetX.value / rowWidthPx >= commitFraction) {
                                onComplete()
                                view.performHapticFeedback(HapticFeedbackConstants.CONFIRM)
                            }
                            isEdgeSwipe = false
                            hasPassedThreshold = false
                            scope.launch { offsetX.animateTo(0f) }
                        },
                        onDragCancel = {
                            isEdgeSwipe = false
                            hasPassedThreshold = false
                            scope.launch { offsetX.animateTo(0f) }
                        },
                        onHorizontalDrag = { _, dragAmount ->
                            if (!isEdgeSwipe) return@detectHorizontalDragGestures
                            scope.launch {
                                val newValue = (offsetX.value + dragAmount).coerceIn(0f, rowWidthPx)
                                offsetX.snapTo(newValue)
                            }
                            // Haptic tick when crossing the commit threshold
                            val nowPastThreshold = offsetX.value / rowWidthPx >= commitFraction
                            if (nowPastThreshold && !hasPassedThreshold) {
                                view.performHapticFeedback(HapticFeedbackConstants.CLOCK_TICK)
                                hasPassedThreshold = true
                            } else if (!nowPastThreshold && hasPassedThreshold) {
                                hasPassedThreshold = false
                            }
                        },
                    )
                }
        ) {
            TodoRow(item = item, onTap = onTap, onLongPress = onLongPress)
        }
    }
}

@OptIn(ExperimentalFoundationApi::class)
@Composable
private fun SelectableTodoRow(
    item: ItemCacheEntity,
    isSelected: Boolean,
    onToggle: () -> Unit,
) {
    val bgColor = if (isSelected)
        MaterialTheme.colorScheme.primaryContainer
    else
        MaterialTheme.colorScheme.surface

    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(bgColor)
            .clickable(onClick = onToggle)
            .padding(horizontal = 16.dp, vertical = 12.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        Icon(
            imageVector = if (isSelected) Icons.Default.CheckCircle else Icons.Default.RadioButtonUnchecked,
            contentDescription = if (isSelected) "Selected" else "Not selected",
            tint = if (isSelected) MaterialTheme.colorScheme.primary else MaterialTheme.colorScheme.onSurfaceVariant,
            modifier = Modifier.size(24.dp),
        )
        Column(modifier = Modifier.weight(1f)) {
            val color = priorityColor(item.priority, item.completed)
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

@OptIn(ExperimentalFoundationApi::class)
@Composable
private fun TodoRow(
    item: ItemCacheEntity,
    onTap: () -> Unit,
    onLongPress: () -> Unit = {},
) {
    val color = priorityColor(item.priority, item.completed)
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .combinedClickable(
                onClick = onTap,
                onLongClick = onLongPress,
            )
            .background(MaterialTheme.colorScheme.surface)
            .padding(horizontal = 16.dp, vertical = 12.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
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

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun FilterSheetContent(
    projects: List<String>,
    contexts: List<String>,
    selectedProjects: MutableList<String>,
    selectedContexts: MutableList<String>,
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
                AssistChip(
                    onClick = onClearAll,
                    label = { Text("Clear all") },
                )
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
