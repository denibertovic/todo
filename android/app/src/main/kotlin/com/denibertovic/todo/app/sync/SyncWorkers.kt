package com.denibertovic.todo.app.sync

import android.content.Context
import androidx.work.CoroutineWorker
import androidx.work.WorkerParameters
import com.denibertovic.todo.app.data.SyncRepository

/**
 * Single worker class shared by both periodic and expedited sync.
 * Dependencies are injected via [TodoWorkerFactory] instead of being
 * created inline, so the worker can be tested with a fake repository.
 */
class SyncWorker(
    appContext: Context,
    params: WorkerParameters,
    private val syncRepository: SyncRepository,
) : CoroutineWorker(appContext, params) {

    override suspend fun doWork(): Result {
        return when (syncRepository.sync()) {
            is SyncRepository.Result.Success -> Result.success()
            is SyncRepository.Result.NotRegistered -> Result.success()
            is SyncRepository.Result.Failure -> Result.retry()
        }
    }
}
