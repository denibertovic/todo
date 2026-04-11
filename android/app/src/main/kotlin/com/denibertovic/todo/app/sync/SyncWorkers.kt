package com.denibertovic.todo.app.sync

import android.content.Context
import androidx.work.CoroutineWorker
import androidx.work.WorkerParameters
import com.denibertovic.todo.app.data.SyncRepository
import com.denibertovic.todo.app.data.db.TodoDatabase

/**
 * Periodic sync heartbeat. Runs at most every 15 minutes (the
 * WorkManager minimum for periodic work) and bails cleanly if the
 * device hasn't been registered yet — this saves us from spinning
 * up workers on a fresh install before onboarding completes.
 */
class PeriodicSyncWorker(
    appContext: Context,
    params: WorkerParameters,
) : CoroutineWorker(appContext, params) {

    override suspend fun doWork(): Result {
        val db = TodoDatabase.get(applicationContext)
        val repo = SyncRepository(db)
        return when (val result = repo.sync()) {
            is SyncRepository.Result.Success -> Result.success()
            is SyncRepository.Result.NotRegistered -> Result.success()
            is SyncRepository.Result.Failure -> Result.retry()
        }
    }
}

/**
 * One-shot expedited sync — fired after every local mutation so
 * changes land on the server as quickly as the OS will let us.
 */
class ExpeditedSyncWorker(
    appContext: Context,
    params: WorkerParameters,
) : CoroutineWorker(appContext, params) {

    override suspend fun doWork(): Result {
        val db = TodoDatabase.get(applicationContext)
        val repo = SyncRepository(db)
        return when (val result = repo.sync()) {
            is SyncRepository.Result.Success -> Result.success()
            is SyncRepository.Result.NotRegistered -> Result.success()
            // Let WorkManager back off and try again later on failure.
            is SyncRepository.Result.Failure -> Result.retry()
        }
    }
}
