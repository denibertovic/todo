package com.denibertovic.todo.app

import android.app.Application
import androidx.work.Configuration
import com.denibertovic.todo.app.data.ExportRepository
import com.denibertovic.todo.app.data.RegistrationRepository
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.TodoActions
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.sync.SyncScheduler
import com.denibertovic.todo.app.sync.TodoWorkerFactory

/**
 * Single place where all long-lived dependencies are wired up.
 * There's no DI framework — the object graph is small enough
 * (database, sync scheduler, a handful of repositories) that an
 * explicit service locator is clearer than hidden injection.
 *
 * Implements [Configuration.Provider] so WorkManager uses our
 * [TodoWorkerFactory] for dependency injection into workers,
 * making them testable with fakes.
 */
class TodoApplication : Application(), Configuration.Provider {

    lateinit var database: TodoDatabase
        private set

    lateinit var syncScheduler: SyncScheduler
        private set

    lateinit var syncRepository: SyncRepository
        private set

    lateinit var registrationRepository: RegistrationRepository
        private set

    lateinit var todoActions: TodoActions
        private set

    lateinit var exportRepository: ExportRepository
        private set

    override fun onCreate() {
        super.onCreate()
        database = TodoDatabase.get(this)
        syncScheduler = SyncScheduler(this)
        syncRepository = SyncRepository(database)
        registrationRepository = RegistrationRepository(database)
        todoActions = TodoActions(database, syncScheduler)
        exportRepository = ExportRepository(database)

        syncScheduler.enqueuePeriodicSync()
    }

    override val workManagerConfiguration: Configuration
        get() = Configuration.Builder()
            .setWorkerFactory(TodoWorkerFactory(database))
            .build()
}
