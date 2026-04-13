package com.denibertovic.todo.app.ui.edit

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.automirrored.filled.Label
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.CalendarMonth
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Code
import androidx.compose.material.icons.filled.Flag
import androidx.compose.material.icons.filled.FolderOpen
import androidx.compose.material.icons.filled.Sell
import androidx.compose.material3.AssistChip
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.DatePicker
import androidx.compose.material3.DatePickerDialog
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.InputChip
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.rememberDatePickerState
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
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.data.displayMetadata
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Priority
import kotlinx.datetime.Instant
import kotlinx.datetime.TimeZone
import kotlinx.datetime.toLocalDateTime

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
fun EditScreen(
    viewModel: EditViewModel,
    onDone: () -> Unit,
) {
    val focusRequester = remember { FocusRequester() }

    val allItems by viewModel.allItems.collectAsState()
    val (allProjects, allContexts) = remember(allItems) {
        val p = sortedSetOf<String>()
        val c = sortedSetOf<String>()
        for (item in allItems) {
            for (m in item.displayMetadata()) {
                when (m) {
                    is Metadata.ProjectM -> p.add(m.project.name)
                    is Metadata.ContextM -> c.add(m.context.name)
                    else -> Unit
                }
            }
        }
        p.toList() to c.toList()
    }

    LaunchedEffect(viewModel.isNew) {
        if (viewModel.isNew) focusRequester.requestFocus()
    }

    var showPrioritySheet by remember { mutableStateOf(false) }
    var showProjectSheet by remember { mutableStateOf(false) }
    var showContextSheet by remember { mutableStateOf(false) }
    var showTagSheet by remember { mutableStateOf(false) }
    var showDatePicker by remember { mutableStateOf(false) }

    // Priority sheet
    if (showPrioritySheet) {
        ModalBottomSheet(
            onDismissRequest = { showPrioritySheet = false },
            sheetState = rememberModalBottomSheetState(),
        ) {
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp)
                    .padding(bottom = 32.dp),
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                Text("Priority", style = MaterialTheme.typography.titleLarge)
                FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    val options = listOf(null) + Priority.entries.take(6)
                    options.forEach { p ->
                        FilterChip(
                            selected = viewModel.priority == p,
                            onClick = {
                                viewModel.priority = p
                                showPrioritySheet = false
                            },
                            label = { Text(p?.name ?: "None") },
                        )
                    }
                }
            }
        }
    }

    // Project picker sheet
    if (showProjectSheet) {
        PickerSheet(
            title = "Projects",
            available = allProjects.filter { it !in viewModel.itemProjects },
            selected = viewModel.itemProjects.toList(),
            onAdd = { viewModel.itemProjects.add(it) },
            onRemove = { viewModel.itemProjects.remove(it) },
            onDismiss = { showProjectSheet = false },
            prefix = "+",
            newLabel = "New project",
        )
    }

    // Context picker sheet
    if (showContextSheet) {
        PickerSheet(
            title = "Contexts",
            available = allContexts.filter { it !in viewModel.itemContexts },
            selected = viewModel.itemContexts.toList(),
            onAdd = { viewModel.itemContexts.add(it) },
            onRemove = { viewModel.itemContexts.remove(it) },
            onDismiss = { showContextSheet = false },
            prefix = "@",
            newLabel = "New context",
        )
    }

    // Tag picker sheet
    if (showTagSheet) {
        TagPickerSheet(
            selected = viewModel.itemTags.toList(),
            onAdd = { k, v -> viewModel.itemTags.add(k to v) },
            onRemove = { viewModel.itemTags.remove(it) },
            onDismiss = { showTagSheet = false },
        )
    }

    // Due date picker
    if (showDatePicker) {
        var dueMode by remember { mutableStateOf(if (viewModel.isDueNext) 1 else 0) }
        val datePickerState = rememberDatePickerState(
            initialSelectedDateMillis = viewModel.dueDate?.let {
                it.toEpochDays().toLong() * 86400000L
            }
        )
        DatePickerDialog(
            onDismissRequest = { showDatePicker = false },
            confirmButton = {
                TextButton(onClick = {
                    if (dueMode == 1) {
                        viewModel.dueDate = null
                        viewModel.isDueNext = true
                    } else {
                        datePickerState.selectedDateMillis?.let { millis ->
                            viewModel.isDueNext = false
                            viewModel.dueDate = Instant.fromEpochMilliseconds(millis)
                                .toLocalDateTime(TimeZone.UTC).date
                        }
                    }
                    showDatePicker = false
                }) { Text("OK") }
            },
            dismissButton = {
                Row {
                    if (viewModel.dueDate != null || viewModel.isDueNext) {
                        TextButton(onClick = {
                            viewModel.dueDate = null
                            viewModel.isDueNext = false
                            showDatePicker = false
                        }) { Text("Clear") }
                    }
                    TextButton(onClick = { showDatePicker = false }) { Text("Cancel") }
                }
            },
        ) {
            Column {
                SingleChoiceSegmentedButtonRow(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(horizontal = 16.dp, vertical = 8.dp),
                ) {
                    SegmentedButton(
                        selected = dueMode == 0,
                        onClick = { dueMode = 0 },
                        shape = SegmentedButtonDefaults.itemShape(index = 0, count = 2),
                    ) { Text("Date") }
                    SegmentedButton(
                        selected = dueMode == 1,
                        onClick = { dueMode = 1 },
                        shape = SegmentedButtonDefaults.itemShape(index = 1, count = 2),
                    ) { Text("Next") }
                }
                if (dueMode == 0) {
                    DatePicker(state = datePickerState)
                } else {
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(360.dp),
                        verticalArrangement = Arrangement.Center,
                        horizontalAlignment = Alignment.CenterHorizontally,
                    ) {
                        Text(
                            "due:next",
                            style = MaterialTheme.typography.headlineSmall,
                            color = MaterialTheme.colorScheme.primary,
                        )
                        Spacer(Modifier.height(8.dp))
                        Text(
                            "Mark as due next time you review your list",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
            }
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                navigationIcon = {
                    IconButton(onClick = onDone) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Back")
                    }
                },
                title = { Text(if (viewModel.isNew) "New todo" else "Edit todo") },
                actions = {
                    IconButton(onClick = viewModel::toggleRawEditor) {
                        Icon(Icons.Default.Code, contentDescription = "Toggle raw editor")
                    }
                    TextButton(
                        enabled = viewModel.canSave,
                        onClick = { viewModel.save(onDone) },
                    ) { Text("Save") }
                },
            )
        },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(horizontal = 16.dp)
                .verticalScroll(rememberScrollState()),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Spacer(Modifier.height(4.dp))

            viewModel.error?.let {
                Text(it, color = MaterialTheme.colorScheme.error)
            }

            // Raw preview (read-only)
            AnimatedVisibility(visible = viewModel.showRaw) {
                OutlinedTextField(
                    value = viewModel.rawText,
                    onValueChange = {},
                    readOnly = true,
                    modifier = Modifier.fillMaxWidth(),
                    label = { Text("Raw todo.txt (preview)") },
                    singleLine = false,
                    minLines = 3,
                )
            }

            // Structured editor
            AnimatedVisibility(visible = !viewModel.showRaw) {
                Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
                    OutlinedTextField(
                        value = viewModel.description,
                        onValueChange = { viewModel.description = it },
                        modifier = Modifier
                            .fillMaxWidth()
                            .focusRequester(focusRequester),
                        label = { Text("Description") },
                        placeholder = { Text("What needs to be done?") },
                        singleLine = false,
                    )

                    Spacer(Modifier.height(4.dp))

                    // Metadata cards — 2x2 grid
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.spacedBy(12.dp),
                    ) {
                        MetadataCard(
                            icon = Icons.Default.Flag,
                            label = "Priority",
                            value = viewModel.priority?.name,
                            isSet = viewModel.priority != null,
                            onClick = { showPrioritySheet = true },
                            modifier = Modifier.weight(1f),
                        )
                        MetadataCard(
                            icon = Icons.Default.FolderOpen,
                            label = "Project",
                            value = if (viewModel.itemProjects.isNotEmpty()) viewModel.itemProjects.joinToString(", ") { "+$it" } else null,
                            isSet = viewModel.itemProjects.isNotEmpty(),
                            onClick = { showProjectSheet = true },
                            modifier = Modifier.weight(1f),
                        )
                    }
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.spacedBy(12.dp),
                    ) {
                        MetadataCard(
                            icon = Icons.AutoMirrored.Filled.Label,
                            label = "Context",
                            value = if (viewModel.itemContexts.isNotEmpty()) viewModel.itemContexts.joinToString(", ") { "@$it" } else null,
                            isSet = viewModel.itemContexts.isNotEmpty(),
                            onClick = { showContextSheet = true },
                            modifier = Modifier.weight(1f),
                        )
                        MetadataCard(
                            icon = Icons.Default.CalendarMonth,
                            label = "Due date",
                            value = when {
                                viewModel.isDueNext -> "due:next"
                                viewModel.dueDate != null -> viewModel.dueDate.toString()
                                else -> null
                            },
                            isSet = viewModel.dueDate != null || viewModel.isDueNext,
                            onClick = { showDatePicker = true },
                            modifier = Modifier.weight(1f),
                        )
                    }
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.spacedBy(12.dp),
                    ) {
                        MetadataCard(
                            icon = Icons.Default.Sell,
                            label = "Tags",
                            value = if (viewModel.itemTags.isNotEmpty()) viewModel.itemTags.joinToString(", ") { "${it.first}:${it.second}" } else null,
                            isSet = viewModel.itemTags.isNotEmpty(),
                            onClick = { showTagSheet = true },
                            modifier = Modifier.weight(1f),
                        )
                        // Empty spacer to keep the grid aligned
                        Spacer(Modifier.weight(1f))
                    }

                    Spacer(Modifier.height(16.dp))
                }
            }
        }
    }
}

@Composable
private fun MetadataCard(
    icon: ImageVector,
    label: String,
    value: String?,
    isSet: Boolean,
    onClick: () -> Unit,
    modifier: Modifier = Modifier,
) {
    val containerColor = if (isSet)
        MaterialTheme.colorScheme.primaryContainer
    else
        MaterialTheme.colorScheme.surfaceVariant
    val contentColor = if (isSet)
        MaterialTheme.colorScheme.onPrimaryContainer
    else
        MaterialTheme.colorScheme.onSurfaceVariant

    Card(
        onClick = onClick,
        modifier = modifier.height(88.dp),
        colors = CardDefaults.cardColors(containerColor = containerColor),
    ) {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(12.dp),
            verticalArrangement = Arrangement.SpaceBetween,
        ) {
            Icon(icon, contentDescription = null, tint = contentColor, modifier = Modifier.size(24.dp))
            Column {
                Text(
                    text = value ?: "Not set",
                    style = MaterialTheme.typography.bodyMedium,
                    color = contentColor,
                    maxLines = 1,
                )
                Text(
                    text = label,
                    style = MaterialTheme.typography.labelSmall,
                    color = contentColor.copy(alpha = 0.7f),
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun PickerSheet(
    title: String,
    available: List<String>,
    selected: List<String>,
    onAdd: (String) -> Unit,
    onRemove: (String) -> Unit,
    onDismiss: () -> Unit,
    prefix: String,
    newLabel: String,
) {
    var newValue by remember { mutableStateOf("") }

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        sheetState = rememberModalBottomSheetState(),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 16.dp)
                .padding(bottom = 32.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(title, style = MaterialTheme.typography.titleLarge)

            if (selected.isNotEmpty()) {
                Text("Selected", style = MaterialTheme.typography.titleSmall)
                FlowRow(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    selected.forEach { item ->
                        InputChip(
                            selected = true,
                            onClick = { onRemove(item) },
                            label = { Text("$prefix$item") },
                            trailingIcon = {
                                Icon(Icons.Default.Close, contentDescription = "Remove")
                            },
                        )
                    }
                }
            }

            if (available.isNotEmpty()) {
                if (selected.isNotEmpty()) HorizontalDivider()
                Text("Available", style = MaterialTheme.typography.titleSmall)
                FlowRow(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    available.forEach { item ->
                        AssistChip(
                            onClick = { onAdd(item) },
                            label = { Text("$prefix$item") },
                        )
                    }
                }
            }

            HorizontalDivider()
            OutlinedTextField(
                value = newValue,
                onValueChange = { newValue = it },
                modifier = Modifier.fillMaxWidth(),
                label = { Text(newLabel) },
                singleLine = true,
                keyboardOptions = KeyboardOptions(imeAction = ImeAction.Done),
                keyboardActions = KeyboardActions(onDone = {
                    val trimmed = newValue.trim()
                    if (trimmed.isNotBlank() && trimmed !in selected) {
                        onAdd(trimmed)
                    }
                    newValue = ""
                }),
                trailingIcon = {
                    if (newValue.isNotBlank()) {
                        IconButton(onClick = {
                            val trimmed = newValue.trim()
                            if (trimmed.isNotBlank() && trimmed !in selected) {
                                onAdd(trimmed)
                            }
                            newValue = ""
                        }) {
                            Icon(Icons.Default.Add, contentDescription = "Add")
                        }
                    }
                },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun TagPickerSheet(
    selected: List<Pair<String, String>>,
    onAdd: (String, String) -> Unit,
    onRemove: (Pair<String, String>) -> Unit,
    onDismiss: () -> Unit,
) {
    var newKey by remember { mutableStateOf("") }
    var newValue by remember { mutableStateOf("") }

    fun addTag() {
        val k = newKey.trim()
        val v = newValue.trim()
        if (k.isNotBlank() && v.isNotBlank() && (k to v) !in selected) {
            onAdd(k, v)
        }
        newKey = ""
        newValue = ""
    }

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        sheetState = rememberModalBottomSheetState(),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 16.dp)
                .padding(bottom = 32.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Tags", style = MaterialTheme.typography.titleLarge)

            if (selected.isNotEmpty()) {
                FlowRow(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    selected.forEach { tag ->
                        InputChip(
                            selected = true,
                            onClick = { onRemove(tag) },
                            label = { Text("${tag.first}:${tag.second}") },
                            trailingIcon = {
                                Icon(Icons.Default.Close, contentDescription = "Remove")
                            },
                        )
                    }
                }
            }

            HorizontalDivider()
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                OutlinedTextField(
                    value = newKey,
                    onValueChange = { newKey = it },
                    modifier = Modifier.weight(1f),
                    label = { Text("Key") },
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(imeAction = ImeAction.Next),
                )
                OutlinedTextField(
                    value = newValue,
                    onValueChange = { newValue = it },
                    modifier = Modifier.weight(1f),
                    label = { Text("Value") },
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(imeAction = ImeAction.Done),
                    keyboardActions = KeyboardActions(onDone = { addTag() }),
                    trailingIcon = {
                        if (newKey.isNotBlank() && newValue.isNotBlank()) {
                            IconButton(onClick = { addTag() }) {
                                Icon(Icons.Default.Add, contentDescription = "Add")
                            }
                        }
                    },
                )
            }
        }
    }
}
