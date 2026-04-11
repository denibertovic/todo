package com.denibertovic.todo.app.data.db

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.Query

@Dao
interface DeviceStateDao {

    @Query("SELECT value FROM device_state WHERE key = :key LIMIT 1")
    suspend fun get(key: String): String?

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun set(entity: DeviceStateEntity)

    suspend fun put(key: String, value: String) = set(DeviceStateEntity(key, value))

    @Query("DELETE FROM device_state WHERE key = :key")
    suspend fun delete(key: String)

    @Query("DELETE FROM device_state")
    suspend fun clear()
}

/** Well-known keys used by the sync layer. Keep in one place so the spelling is canonical. */
object DeviceStateKeys {
    const val DEVICE_ID   = "device_id"
    const val AUTH_TOKEN  = "auth_token"
    const val CURSOR      = "cursor"
    const val SERVER_URL  = "server_url"
    const val LAST_SYNC   = "last_sync_at"
    const val DEVICE_NAME = "device_name"
}
