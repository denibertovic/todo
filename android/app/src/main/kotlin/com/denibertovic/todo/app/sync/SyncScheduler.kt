package com.denibertovic.todo.app.sync

import android.content.Context
import androidx.work.BackoffPolicy
import androidx.work.Constraints
import androidx.work.ExistingPeriodicWorkPolicy
import androidx.work.ExistingWorkPolicy
import androidx.work.NetworkType
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.OutOfQuotaPolicy
import androidx.work.PeriodicWorkRequestBuilder
import androidx.work.WorkManager
import java.util.concurrent.TimeUnit

/**
 * Wraps WorkManager with the two sync flavors the app uses:
 *
 * - **Periodic**: a 15-minute heartbeat to catch remote changes
 *   when the app hasn't been opened in a while. Needs network,
 *   backs off on failure.
 * - **Expedited one-shot**: fired after every local mutation
 *   (`TodoActions.*`) and on app resume. Uses
 *   `OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST` so it
 *   degrades gracefully when we've burned through the expedited
 *   budget.
 *
 * Both jobs share the same network-required constraint so we don't
 * spin up a worker just to immediately crash on
 * `ConnectException`.
 */
class SyncScheduler(private val context: Context) {

    fun enqueuePeriodicSync() {
        val constraints = Constraints.Builder()
            .setRequiredNetworkType(NetworkType.CONNECTED)
            .build()
        val request = PeriodicWorkRequestBuilder<PeriodicSyncWorker>(15, TimeUnit.MINUTES)
            .setConstraints(constraints)
            .setBackoffCriteria(BackoffPolicy.EXPONENTIAL, 30, TimeUnit.SECONDS)
            .build()
        WorkManager.getInstance(context).enqueueUniquePeriodicWork(
            PERIODIC_WORK_NAME,
            ExistingPeriodicWorkPolicy.KEEP,
            request,
        )
    }

    fun enqueueExpeditedSync() {
        val constraints = Constraints.Builder()
            .setRequiredNetworkType(NetworkType.CONNECTED)
            .build()
        val request = OneTimeWorkRequestBuilder<ExpeditedSyncWorker>()
            .setConstraints(constraints)
            .setExpedited(OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST)
            .setBackoffCriteria(BackoffPolicy.EXPONENTIAL, 15, TimeUnit.SECONDS)
            .build()
        WorkManager.getInstance(context).enqueueUniqueWork(
            EXPEDITED_WORK_NAME,
            ExistingWorkPolicy.REPLACE,
            request,
        )
    }

    fun cancelAll() {
        WorkManager.getInstance(context).cancelUniqueWork(PERIODIC_WORK_NAME)
        WorkManager.getInstance(context).cancelUniqueWork(EXPEDITED_WORK_NAME)
    }

    companion object {
        private const val PERIODIC_WORK_NAME = "todo-periodic-sync"
        private const val EXPEDITED_WORK_NAME = "todo-expedited-sync"
    }
}
