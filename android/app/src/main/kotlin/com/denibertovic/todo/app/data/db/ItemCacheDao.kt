package com.denibertovic.todo.app.data.db

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.Query
import androidx.room.Transaction
import kotlinx.coroutines.flow.Flow

@Dao
interface ItemCacheDao {

    /**
     * Live view of all non-deleted items. Emits a new list whenever
     * the cache table changes, which is whenever a sync finishes or
     * the user kicks off a local action — WorkManager and
     * `TodoActions` both insert into this table inside the same
     * transaction as their op-log write, so the UI state stays in
     * lockstep with the CRDT.
     */
    @Query("SELECT * FROM items_cache WHERE deleted = 0 ORDER BY createdAt DESC")
    fun observeVisible(): Flow<List<ItemCacheEntity>>

    @Query("SELECT * FROM items_cache WHERE itemId = :itemId")
    suspend fun getById(itemId: String): ItemCacheEntity?

    /**
     * Replace the cache wholesale with a freshly-materialized set.
     * Called from [com.denibertovic.todo.app.data.SyncRepository]
     * after every sync round — cheaper than a diff for the problem
     * sizes we're targeting.
     */
    @Transaction
    suspend fun replaceAll(items: List<ItemCacheEntity>) {
        deleteAll()
        insertAll(items)
    }

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun insertAll(items: List<ItemCacheEntity>)

    @Query("DELETE FROM items_cache")
    suspend fun deleteAll()
}
