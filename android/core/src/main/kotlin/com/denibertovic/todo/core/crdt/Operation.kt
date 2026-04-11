package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.OperationId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.TodoItem
import kotlinx.datetime.Instant
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * A single CRDT operation. Every local user action and every remote
 * sync delivery is an `Operation` — the op log is the canonical state
 * and materialized items are recomputed from it.
 *
 * Wire shape matches `ToJSON Operation` in
 * `src/Todo/Sync/Types.hs:174-189`: common fields (`op_id`, `item_id`,
 * `timestamp`, `device_id`) are always present, plus a `type`
 * discriminator and variant-specific fields.
 */
@Serializable
sealed class Operation {
    /** UUID uniquely identifying this operation across all devices. */
    abstract val opId: OperationId

    /** The todo item this operation targets. */
    abstract val itemId: ItemId

    /** When the operation was generated. Drives the CRDT ordering. */
    abstract val timestamp: Instant

    /** Which device generated this operation. LWW tiebreaker. */
    abstract val deviceId: DeviceId

    /** Create a brand-new todo item at a known item ID. */
    @Serializable
    @SerialName("add")
    data class Add(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
        val item: TodoItem,
    ) : Operation()

    /** Mark an item as completed. Complete-wins vs uncomplete. */
    @Serializable
    @SerialName("complete")
    data class Complete(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
    ) : Operation()

    /** Mark an item as incomplete again. Loses against a Complete. */
    @Serializable
    @SerialName("uncomplete")
    data class Uncomplete(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
    ) : Operation()

    /** Mark an item as deleted. Delete-wins — never resurrected. */
    @Serializable
    @SerialName("delete")
    data class Delete(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
    ) : Operation()

    /** Change the priority on an item. LWW semantics. */
    @Serializable
    @SerialName("set_priority")
    data class SetPriority(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
        val priority: Priority?,
    ) : Operation()

    /** Change the description text on an item. LWW semantics. */
    @Serializable
    @SerialName("modify_description")
    data class ModifyDescription(
        @SerialName("op_id") override val opId: OperationId,
        @SerialName("item_id") override val itemId: ItemId,
        override val timestamp: Instant,
        @SerialName("device_id") override val deviceId: DeviceId,
        val description: String,
    ) : Operation()
}
