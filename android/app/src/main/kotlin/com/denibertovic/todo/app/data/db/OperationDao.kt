package com.denibertovic.todo.app.data.db

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.Query

@Dao
interface OperationDao {

    /**
     * Insert a batch of operations. Conflicts on `opId` are ignored —
     * the CRDT is designed around idempotent replay, so seeing the
     * same op twice from server pagination is a no-op.
     */
    @Insert(onConflict = OnConflictStrategy.IGNORE)
    suspend fun insertAll(ops: List<OperationEntity>)

    /** Pending ops are ones we created locally that have not yet been acked by the server. */
    @Query("SELECT * FROM operations WHERE isPending = 1 ORDER BY createdAt ASC")
    suspend fun getPending(): List<OperationEntity>

    /** Every operation we've ever seen, including synced ones. Used for full materialization. */
    @Query("SELECT * FROM operations ORDER BY createdAt ASC")
    suspend fun getAll(): List<OperationEntity>

    /** Flip the pending flag on a set of op IDs. Called after a successful sync round. */
    @Query("UPDATE operations SET isPending = 0 WHERE opId IN (:opIds)")
    suspend fun clearPendingFlag(opIds: List<String>)

    @Query("SELECT COUNT(*) FROM operations WHERE isPending = 1")
    suspend fun pendingCount(): Int
}
