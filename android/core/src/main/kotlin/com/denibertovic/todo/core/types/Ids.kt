package com.denibertovic.todo.core.types

import com.denibertovic.todo.core.json.DeviceIdSerializer
import com.denibertovic.todo.core.json.ItemIdSerializer
import com.denibertovic.todo.core.json.OperationIdSerializer
import kotlinx.serialization.Serializable
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

/**
 * Typed UUID wrappers for sync identifiers. These are distinct types
 * rather than bare [Uuid] to prevent accidentally passing a device ID
 * where an item ID is expected (the kind of mistake that compiles and
 * runs but silently corrupts the CRDT).
 *
 * Serialization: always the canonical lowercase hex-dash form, to
 * match `UUID.toText` on the Haskell side.
 */
@OptIn(ExperimentalUuidApi::class)
@Serializable(with = ItemIdSerializer::class)
@JvmInline
value class ItemId(val value: Uuid) {
    override fun toString(): String = value.toString()

    companion object {
        fun random(): ItemId = ItemId(Uuid.random())
        fun parse(text: String): ItemId = ItemId(Uuid.parse(text))
    }
}

/**
 * Device identifier. [Comparable] so the CRDT's LWW tiebreaker can
 * use it: when two operations have identical timestamps, the one
 * from the device with the lexicographically-greater ID wins. This
 * matches `mergeLWW` in `src/Todo/Sync/Types.hs:241`.
 */
@OptIn(ExperimentalUuidApi::class)
@Serializable(with = DeviceIdSerializer::class)
@JvmInline
value class DeviceId(val value: Uuid) : Comparable<DeviceId> {
    override fun toString(): String = value.toString()
    override fun compareTo(other: DeviceId): Int = value.toString().compareTo(other.value.toString())

    companion object {
        fun random(): DeviceId = DeviceId(Uuid.random())
        fun parse(text: String): DeviceId = DeviceId(Uuid.parse(text))
    }
}

@OptIn(ExperimentalUuidApi::class)
@Serializable(with = OperationIdSerializer::class)
@JvmInline
value class OperationId(val value: Uuid) : Comparable<OperationId> {
    override fun toString(): String = value.toString()
    override fun compareTo(other: OperationId): Int = value.toString().compareTo(other.value.toString())

    companion object {
        fun random(): OperationId = OperationId(Uuid.random())
        fun parse(text: String): OperationId = OperationId(Uuid.parse(text))
    }
}
