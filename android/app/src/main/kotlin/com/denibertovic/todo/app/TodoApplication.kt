package com.denibertovic.todo.app

import android.app.Application
import com.denibertovic.todo.app.data.ExportRepository
import com.denibertovic.todo.app.data.RegistrationRepository
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.TodoActions
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.sync.SyncScheduler

/**
 * Single place where all long-lived dependencies are wired up.
 * There's no DI framework — the object graph is small enough
 * (database, sync scheduler, a handful of repositories) that an
 * explicit service locator is clearer than hidden injection.
 */
class TodoApplication : Application() {

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

        // Schedule the periodic heartbeat once per process launch.
        // WorkManager de-duplicates by unique work name so this is
        // idempotent — it's safe to call every cold start.
        syncScheduler.enqueuePeriodicSync()
    }
}
