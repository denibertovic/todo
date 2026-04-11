package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * Last-Writer-Wins register. Used for fields (priority, description)
 * where concurrent edits need deterministic merging.
 *
 * Wire shape and merge semantics match
 * `src/Todo/Sync/Types.hs:220-245`. When timestamps are equal, the
 * tiebreaker is the device ID — lexicographically greater wins.
 */
@Serializable
data class LWWRegister<T>(
    @SerialName("value") val value: T,
    @SerialName("timestamp") val timestamp: Instant,
    @SerialName("device_id") val deviceId: DeviceId,
)

/**
 * Merge two LWW registers — most recent timestamp wins, device ID is
 * the tiebreaker. Mirrors `mergeLWW` at
 * `src/Todo/Sync/Types.hs:241-245`.
 */
fun <T> LWWRegister<T>.merge(other: LWWRegister<T>): LWWRegister<T> = when {
    timestamp > other.timestamp -> this
    timestamp < other.timestamp -> other
    deviceId > other.deviceId   -> this
    else                         -> other
}
