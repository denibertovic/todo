package com.denibertovic.todo.app.ui.edit

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.TodoActions
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.data.displayDescription
import com.denibertovic.todo.app.data.displayMetadata
import com.denibertovic.todo.core.parser.TodoParser
import com.denibertovic.todo.core.types.Context
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.Project
import com.denibertovic.todo.core.types.Tag
import com.denibertovic.todo.core.types.TodoItem
import com.denibertovic.todo.core.types.renderItem
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch
import kotlinx.datetime.LocalDate

class EditViewModel(
    private val database: TodoDatabase,
    private val todoActions: TodoActions,
    private val itemId: String?,
) : ViewModel() {

    var description by mutableStateOf("")
    var priority by mutableStateOf<Priority?>(null)
    val itemProjects = mutableStateListOf<String>()
    val itemContexts = mutableStateListOf<String>()
    var dueDate by mutableStateOf<LocalDate?>(null)

    var showRaw by mutableStateOf(false)
        private set
    var rawText by mutableStateOf("")

    var error by mutableStateOf<String?>(null)
        private set

    val canSave: Boolean get() = description.isNotBlank()
    val isNew: Boolean get() = itemId == null

    val allItems: StateFlow<List<ItemCacheEntity>> = database.itemCacheDao().observeVisible()
        .stateIn(viewModelScope, SharingStarted.WhileSubscribed(5000), emptyList())

    init {
        if (itemId != null) loadItem()
    }

    private fun loadItem() {
        viewModelScope.launch {
            val row = database.itemCacheDao().getById(itemId!!) ?: return@launch
            description = row.displayDescription()
            priority = row.priority?.let { Priority.fromLetter(it[0]) }
            val meta = row.displayMetadata()
            itemProjects.addAll(meta.filterIsInstance<Metadata.ProjectM>().map { it.project.name })
            itemContexts.addAll(meta.filterIsInstance<Metadata.ContextM>().map { it.context.name })
            dueDate = meta.firstNotNullOfOrNull { md ->
                (md as? Metadata.TagM)?.tag?.let { it as? Tag.DueDate }?.date
            }
        }
    }

    fun toggleRawEditor() {
        if (!showRaw) syncToRaw()
        showRaw = !showRaw
        if (!showRaw) syncFromRaw()
    }

    private fun syncToRaw() {
        rawText = buildItem().renderItem()
    }

    private fun syncFromRaw() {
        val parsed = TodoParser.parseLine(rawText).getOrNull() ?: return
        val item = parsed.item
        description = item.description
        priority = item.priority
        itemProjects.clear()
        itemProjects.addAll(item.metadata.filterIsInstance<Metadata.ProjectM>().map { it.project.name })
        itemContexts.clear()
        itemContexts.addAll(item.metadata.filterIsInstance<Metadata.ContextM>().map { it.context.name })
        dueDate = item.metadata.firstNotNullOfOrNull { md ->
            (md as? Metadata.TagM)?.tag?.let { it as? Tag.DueDate }?.date
        }
    }

    private fun buildItem(): TodoItem {
        val metadata = buildList {
            itemProjects.forEach { add(Metadata.ProjectM(Project(it))) }
            itemContexts.forEach { add(Metadata.ContextM(Context(it))) }
            dueDate?.let { add(Metadata.TagM(Tag.DueDate(it))) }
        }
        return TodoItem(
            priority = priority,
            description = description.trim(),
            metadata = metadata,
        )
    }

    fun save(onDone: () -> Unit) {
        viewModelScope.launch {
            try {
                val item = buildItem()
                if (itemId == null) {
                    todoActions.add(item)
                } else {
                    val id = ItemId.parse(itemId)
                    // Include metadata tokens in the description so changes
                    // to projects, contexts, and due dates are persisted.
                    // The CRDT has no dedicated metadata-update operation,
                    // so we embed them in the description LWW register.
                    val contentParts = buildList {
                        add(item.description)
                        item.metadata.forEach { add(it.render()) }
                    }
                    todoActions.modifyDescription(id, contentParts.joinToString(" "))
                    todoActions.setPriority(id, item.priority)
                }
                onDone()
            } catch (e: Exception) {
                error = "Save failed: ${e.message}"
            }
        }
    }

    class Factory(
        private val app: TodoApplication,
        private val itemId: String?,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            EditViewModel(app.database, app.todoActions, itemId) as T
    }
}
