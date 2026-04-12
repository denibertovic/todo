package com.denibertovic.todo.app.data.db

import android.content.Context
import androidx.room.Database
import androidx.room.Room
import androidx.room.RoomDatabase

/**
 * Room database for the operation log and derived caches. A single
 * DB file backs everything: the op log, the materialized items
 * cache, and the device state key-value store. Keeping them in one
 * DB lets us advance the sync cursor and insert new operations in a
 * single transaction, so a crash in the middle of a sync leaves the
 * client in a consistent replayable state.
 */
@Database(
    entities = [
        OperationEntity::class,
        ItemCacheEntity::class,
        DeviceStateEntity::class,
    ],
    version = 1,
    exportSchema = true,
)
abstract class TodoDatabase : RoomDatabase() {
    abstract fun operationDao(): OperationDao
    abstract fun itemCacheDao(): ItemCacheDao
    abstract fun deviceStateDao(): DeviceStateDao

    companion object {
        @Volatile private var instance: TodoDatabase? = null

        fun get(context: Context): TodoDatabase =
            instance ?: synchronized(this) {
                instance ?: Room.databaseBuilder(
                    context.applicationContext,
                    TodoDatabase::class.java,
                    "todo.db",
                )
                    // No destructive migrations on upgrades — we
                    // want to preserve the op log across schema
                    // changes. Room-defined migrations are
                    // registered here when needed.
                    .build()
                    .also { instance = it }
            }

        /** For testing / wipe-local-state — tears down the singleton. */
        fun reset() {
            synchronized(this) {
                instance?.close()
                instance = null
            }
        }
    }
}
