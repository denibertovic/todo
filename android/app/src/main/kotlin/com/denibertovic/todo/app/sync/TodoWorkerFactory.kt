package com.denibertovic.todo.app.sync

import android.content.Context
import androidx.work.ListenableWorker
import androidx.work.WorkerFactory
import androidx.work.WorkerParameters
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.TodoDatabase

/**
 * Custom [WorkerFactory] that injects [SyncRepository] into
 * [SyncWorker]. This makes the worker testable without touching the
 * database singleton and keeps dependency creation out of `doWork()`.
 */
class TodoWorkerFactory(private val database: TodoDatabase) : WorkerFactory() {

    override fun createWorker(
        appContext: Context,
        workerClassName: String,
        workerParameters: WorkerParameters,
    ): ListenableWorker? {
        return when (workerClassName) {
            SyncWorker::class.java.name ->
                SyncWorker(appContext, workerParameters, SyncRepository(database))
            else -> null
        }
    }
}
