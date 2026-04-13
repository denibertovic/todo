package com.denibertovic.todo.app.ui.list

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.denibertovic.todo.app.TodoApplication
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.TodoActions
import com.denibertovic.todo.app.data.db.ItemCacheEntity
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.core.types.ItemId
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.SharedFlow
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch

class ListViewModel(
    private val database: TodoDatabase,
    private val todoActions: TodoActions,
    private val syncRepository: SyncRepository,
) : ViewModel() {

    val items: StateFlow<List<ItemCacheEntity>> = database.itemCacheDao().observeVisible()
        .stateIn(viewModelScope, SharingStarted.WhileSubscribed(5000), emptyList())

    // Filter state — survives configuration changes
    val selectedProjects = mutableStateListOf<String>()
    val selectedContexts = mutableStateListOf<String>()
    var dueFilter by mutableStateOf(DueFilter.NONE)
    val activeFilterCount: Int get() = selectedProjects.size + selectedContexts.size + if (dueFilter != DueFilter.NONE) 1 else 0

    enum class DueFilter { NONE, TODAY, OVERDUE, HAS_DUE_DATE }

    // Multi-select state
    val selectedItems = mutableStateListOf<String>()
    val inSelectionMode: Boolean get() = selectedItems.isNotEmpty()

    // Pull-to-refresh
    var isRefreshing by mutableStateOf(false)
        private set

    // One-shot events
    private val _events = MutableSharedFlow<Event>(extraBufferCapacity = 5)
    val events: SharedFlow<Event> = _events

    fun sync() {
        if (inSelectionMode) return
        isRefreshing = true
        viewModelScope.launch {
            try {
                val result = syncRepository.sync()
                val message = when (result) {
                    is SyncRepository.Result.Success ->
                        "Synced: ${result.sent} sent, ${result.received} received"
                    is SyncRepository.Result.NotRegistered ->
                        "Not registered with sync server"
                    is SyncRepository.Result.Failure ->
                        "Sync failed: ${result.error.message}"
                }
                _events.tryEmit(Event.Toast(message))
            } catch (e: Exception) {
                _events.tryEmit(Event.Toast("Sync failed: ${e.message}"))
            } finally {
                isRefreshing = false
            }
        }
    }

    fun toggleComplete(itemId: String, isCompleted: Boolean) {
        viewModelScope.launch {
            try {
                val id = ItemId.parse(itemId)
                if (isCompleted) todoActions.uncomplete(id) else todoActions.complete(id)
            } catch (e: Exception) {
                _events.tryEmit(Event.Toast("Action failed: ${e.message}"))
            }
        }
    }

    fun deleteSelected() {
        val toDelete = selectedItems.toList()
        selectedItems.clear()
        viewModelScope.launch {
            try {
                for (id in toDelete) {
                    todoActions.delete(ItemId.parse(id))
                }
                _events.tryEmit(Event.Toast("Deleted ${toDelete.size} items"))
            } catch (e: Exception) {
                _events.tryEmit(Event.Toast("Delete failed: ${e.message}"))
            }
        }
    }

    fun clearSelection() = selectedItems.clear()

    fun toggleItemSelection(itemId: String) {
        if (itemId in selectedItems) selectedItems.remove(itemId) else selectedItems.add(itemId)
    }

    fun startSelection(itemId: String) {
        selectedItems.add(itemId)
    }

    fun toggleProjectFilter(project: String) {
        if (project in selectedProjects) selectedProjects.remove(project) else selectedProjects.add(project)
    }

    fun toggleContextFilter(context: String) {
        if (context in selectedContexts) selectedContexts.remove(context) else selectedContexts.add(context)
    }

    fun toggleDueFilter(filter: DueFilter) {
        dueFilter = if (dueFilter == filter) DueFilter.NONE else filter
    }

    fun clearFilters() {
        selectedProjects.clear()
        selectedContexts.clear()
        dueFilter = DueFilter.NONE
    }

    sealed class Event {
        data class Toast(val message: String) : Event()
    }

    class Factory(private val app: TodoApplication) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            ListViewModel(app.database, app.todoActions, app.syncRepository) as T
    }
}
