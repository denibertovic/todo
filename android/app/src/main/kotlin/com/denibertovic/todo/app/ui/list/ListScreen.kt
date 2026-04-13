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
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Size
import android.widget.Toast
import com.denibertovic.todo.app.ui.theme.TodoColors
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.displayMetadata
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Tag
import kotlinx.datetime.Clock
import kotlinx.datetime.LocalDate
import kotlinx.datetime.TimeZone
import kotlinx.datetime.todayIn

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

    // Items currently animating out via swipe-to-complete. They are
    // hidden from the list so LazyColumn never sees their key move to
    // a new sort position (which would cause an unwanted scroll jump).
    // Cleared when `items` changes, at which point the item reappears
    // silently at its new position off-screen.
    val dismissingIds = remember { mutableStateListOf<String>() }
    LaunchedEffect(items) { dismissingIds.clear() }

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
    val today = remember { Clock.System.todayIn(TimeZone.currentSystemDefault()) }
    val itemsWithMeta = remember(items) {
        items.map { item ->
            val meta = item.displayMetadata()
            ItemWithMeta(
                entity = item,
                metadata = meta,
                summary = metadataSummary(meta, excludeDue = true),
                dueDate = extractDueDate(meta),
                isDueNext = hasDueNext(meta),
                dueLabel = extractDueLabel(meta),
            )
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
        viewModel.dueFilter,
        dismissingIds.toList(),
    ) {
        val projSet = viewModel.selectedProjects.toSet()
        val ctxSet = viewModel.selectedContexts.toSet()
        val dismissSet = dismissingIds.toSet()
        itemsWithMeta
            .filter { it.entity.itemId !in dismissSet }
            .filter { matchesFilters(it, projSet, ctxSet) }
            .let { list ->
                when (viewModel.dueFilter) {
                    ListViewModel.DueFilter.NONE -> list
                    ListViewModel.DueFilter.TODAY -> list.filter { !it.entity.completed &&
                        (it.dueDate?.let { d -> d <= today } == true || it.isDueNext) }
                    ListViewModel.DueFilter.OVERDUE -> list.filter { !it.entity.completed &&
                        it.dueDate?.let { d -> d < today } == true }
                    ListViewModel.DueFilter.HAS_DUE_DATE -> list.filter { !it.entity.completed &&
                        (it.dueDate != null || it.isDueNext) }
                }
            }
            .sortedWith(listWithMetaOrdering(today))
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
                dueFilter = viewModel.dueFilter,
                onToggleProject = viewModel::toggleProjectFilter,
                onToggleContext = viewModel::toggleContextFilter,
                onToggleDueFilter = viewModel::toggleDueFilter,
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
                        if (viewModel.dueFilter != ListViewModel.DueFilter.NONE) {
                            val label = when (viewModel.dueFilter) {
                                ListViewModel.DueFilter.TODAY -> "Today"
                                ListViewModel.DueFilter.OVERDUE -> "Overdue"
                                ListViewModel.DueFilter.HAS_DUE_DATE -> "Has due date"
                                else -> ""
                            }
                            AssistChip(
                                onClick = { viewModel.toggleDueFilter(viewModel.dueFilter) },
                                label = { Text(label) },
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
                        val isUrgent = !item.completed &&
                            (iwm.dueDate?.let { it <= today } == true || iwm.isDueNext)
                        val isOverdue = !item.completed &&
                            iwm.dueDate?.let { it < today } == true
                        if (viewModel.inSelectionMode) {
                            SelectableTodoRow(
                                item = item,
                                metaSummary = iwm.summary,
                                isSelected = item.itemId in viewModel.selectedItems,
                                isUrgent = isUrgent,
                                isOverdue = isOverdue,
                                dueLabel = iwm.dueLabel,
                                onToggle = { viewModel.toggleItemSelection(item.itemId) },
                            )
                        } else {
                            SwipeableRow(
                                itemId = item.itemId,
                                isCompleted = item.completed,
                                onComplete = {
                                    dismissingIds.add(item.itemId)
                                    viewModel.toggleComplete(item.itemId, item.completed)
                                },
                            ) {
                                TodoRow(
                                    item = item,
                                    metaSummary = iwm.summary,
                                    isUrgent = isUrgent,
                                    isOverdue = isOverdue,
                                    dueLabel = iwm.dueLabel,
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
    isUrgent: Boolean,
    isOverdue: Boolean,
    dueLabel: String?,
    onToggle: () -> Unit,
) {
    val containerColor = if (isSelected)
        MaterialTheme.colorScheme.primaryContainer
    else
        MaterialTheme.colorScheme.surfaceContainer
    val urgentColor = if (isOverdue) TodoColors.overdue() else TodoColors.dueToday()

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = containerColor),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .then(if (isUrgent) Modifier.drawBehind {
                    drawRect(urgentColor, size = Size(4.dp.toPx(), size.height))
                } else Modifier)
                .clickable(onClick = onToggle)
                .padding(
                    start = if (isUrgent) 20.dp else 16.dp,
                    end = 16.dp, top = 14.dp, bottom = 14.dp,
                ),
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
                isUrgent = isUrgent,
                isOverdue = isOverdue,
                dueLabel = dueLabel,
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
    isUrgent: Boolean,
    isOverdue: Boolean,
    dueLabel: String?,
    onTap: () -> Unit,
    onLongPress: () -> Unit = {},
) {
    val urgentColor = if (isOverdue) TodoColors.overdue() else TodoColors.dueToday()
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
                .then(if (isUrgent) Modifier.drawBehind {
                    drawRect(urgentColor, size = Size(4.dp.toPx(), size.height))
                } else Modifier)
                .combinedClickable(onClick = onTap, onLongClick = onLongPress)
                .padding(
                    start = if (isUrgent) 20.dp else 16.dp,
                    end = 16.dp, top = 14.dp, bottom = 14.dp,
                ),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            TodoItemContent(
                description = item.description,
                priority = item.priority,
                completed = item.completed,
                metaSummary = metaSummary,
                isUrgent = isUrgent,
                isOverdue = isOverdue,
                dueLabel = dueLabel,
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
    dueFilter: ListViewModel.DueFilter,
    onToggleProject: (String) -> Unit,
    onToggleContext: (String) -> Unit,
    onToggleDueFilter: (ListViewModel.DueFilter) -> Unit,
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
            if (selectedProjects.isNotEmpty() || selectedContexts.isNotEmpty() || dueFilter != ListViewModel.DueFilter.NONE) {
                AssistChip(onClick = onClearAll, label = { Text("Clear all") })
            }
        }

        Text(
            "Due dates",
            style = MaterialTheme.typography.titleSmall,
            modifier = Modifier.padding(top = 16.dp, bottom = 8.dp),
        )
        FlowRow(
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            FilterChip(
                selected = dueFilter == ListViewModel.DueFilter.TODAY,
                onClick = { onToggleDueFilter(ListViewModel.DueFilter.TODAY) },
                label = { Text("Today") },
            )
            FilterChip(
                selected = dueFilter == ListViewModel.DueFilter.OVERDUE,
                onClick = { onToggleDueFilter(ListViewModel.DueFilter.OVERDUE) },
                label = { Text("Overdue") },
            )
            FilterChip(
                selected = dueFilter == ListViewModel.DueFilter.HAS_DUE_DATE,
                onClick = { onToggleDueFilter(ListViewModel.DueFilter.HAS_DUE_DATE) },
                label = { Text("Has due date") },
            )
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
    val dueDate: LocalDate? = null,
    val isDueNext: Boolean = false,
    val dueLabel: String? = null,
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

internal fun metadataSummary(metadata: List<Metadata>, excludeDue: Boolean = false): String =
    metadata
        .filter { m ->
            !(excludeDue && m is Metadata.TagM && (m.tag is Tag.DueDate || m.tag is Tag.Next))
        }
        .joinToString(" ") { m ->
            when (m) {
                is Metadata.ProjectM -> "+${m.project.name}"
                is Metadata.ContextM -> "@${m.context.name}"
                is Metadata.TagM -> m.tag.render()
                is Metadata.Str -> m.value
                is Metadata.LinkM -> m.link.url
            }
        }

internal fun listWithMetaOrdering(today: LocalDate) = Comparator<ItemWithMeta> { a, b ->
    val completeCmp = a.entity.completed.compareTo(b.entity.completed)
    if (completeCmp != 0) return@Comparator completeCmp

    val urgentA = !a.entity.completed && (a.dueDate?.let { it <= today } == true || a.isDueNext)
    val urgentB = !b.entity.completed && (b.dueDate?.let { it <= today } == true || b.isDueNext)
    if (urgentA && !urgentB) return@Comparator -1
    if (!urgentA && urgentB) return@Comparator 1
    if (urgentA && urgentB) {
        val da = a.dueDate
        val db = b.dueDate
        if (da == null && db != null) return@Comparator -1  // due:next before dated
        if (da != null && db == null) return@Comparator 1
        if (da != null && db != null) {
            val dateCmp = da.compareTo(db)
            if (dateCmp != 0) return@Comparator dateCmp
        }
    }

    val priCmp = priorityRank(a.entity.priority).compareTo(priorityRank(b.entity.priority))
    if (priCmp != 0) return@Comparator priCmp
    b.entity.createdAt.compareTo(a.entity.createdAt)
}

private fun priorityRank(priority: String?): Int =
    if (priority == null) Int.MAX_VALUE else priority[0] - 'A'

private fun extractDueDate(metadata: List<Metadata>): LocalDate? =
    metadata.filterIsInstance<Metadata.TagM>()
        .mapNotNull { (it.tag as? Tag.DueDate)?.date }
        .firstOrNull()

private fun hasDueNext(metadata: List<Metadata>): Boolean =
    metadata.any { it is Metadata.TagM && it.tag is Tag.Next }

private fun extractDueLabel(metadata: List<Metadata>): String? =
    metadata.filterIsInstance<Metadata.TagM>()
        .firstNotNullOfOrNull { m ->
            when (m.tag) {
                is Tag.DueDate -> m.tag.render()
                is Tag.Next -> m.tag.render()
                else -> null
            }
        }
