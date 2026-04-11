package com.denibertovic.todo.core.json

import com.denibertovic.todo.core.types.Context
import com.denibertovic.todo.core.types.DeviceId
import com.denibertovic.todo.core.types.ItemId
import com.denibertovic.todo.core.types.Link
import com.denibertovic.todo.core.types.OperationId
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.Project
import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.Json
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

/**
 * Shared [Json] instance used for all sync wire-format encoding and
 * decoding. It matches the shape produced by the Haskell server's
 * Aeson instances: snake-case field names, a `type` discriminator for
 * sealed hierarchies, no pretty-printing.
 *
 * Keeping a single configured [Json] here prevents accidentally
 * creating a new instance in a hot path (which is expensive) and
 * ensures every call site agrees on the encoding.
 */
val TodoJson: Json = Json {
    ignoreUnknownKeys = true           // tolerate newer server fields
    encodeDefaults = true              // Aeson always emits `null` fields
    explicitNulls = true
    classDiscriminator = "type"        // matches Haskell Operation/Metadata/Tag
    // Note: the `Todo` sealed class uses a different discriminator
    // ("status"), which is overridden locally via @JsonClassDiscriminator.
}

/**
 * String-based serializer for [Priority]. Matches
 * `ToJSON Priority` at `src/Todo/Types.hs:325` which emits a single
 * uppercase letter, not the bracket form used for rendering to
 * `todo.txt`.
 */
object PrioritySerializer : KSerializer<Priority> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("Priority", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: Priority) {
        encoder.encodeString(value.name)
    }

    override fun deserialize(decoder: Decoder): Priority {
        val s = decoder.decodeString()
        require(s.length == 1) { "Priority must be a single letter A-Z, got $s" }
        return Priority.fromLetter(s[0])
            ?: throw IllegalArgumentException("Invalid priority: $s")
    }
}

object ProjectSerializer : KSerializer<Project> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("Project", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: Project) = encoder.encodeString(value.name)
    override fun deserialize(decoder: Decoder): Project = Project(decoder.decodeString())
}

object ContextSerializer : KSerializer<Context> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("Context", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: Context) = encoder.encodeString(value.name)
    override fun deserialize(decoder: Decoder): Context = Context(decoder.decodeString())
}

object LinkSerializer : KSerializer<Link> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("Link", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: Link) = encoder.encodeString(value.url)
    override fun deserialize(decoder: Decoder): Link = Link(decoder.decodeString())
}

@OptIn(ExperimentalUuidApi::class)
object ItemIdSerializer : KSerializer<ItemId> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("ItemId", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: ItemId) =
        encoder.encodeString(value.value.toString())
    override fun deserialize(decoder: Decoder): ItemId =
        ItemId(Uuid.parse(decoder.decodeString()))
}

@OptIn(ExperimentalUuidApi::class)
object DeviceIdSerializer : KSerializer<DeviceId> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("DeviceId", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: DeviceId) =
        encoder.encodeString(value.value.toString())
    override fun deserialize(decoder: Decoder): DeviceId =
        DeviceId(Uuid.parse(decoder.decodeString()))
}

@OptIn(ExperimentalUuidApi::class)
object OperationIdSerializer : KSerializer<OperationId> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("OperationId", PrimitiveKind.STRING)
    override fun serialize(encoder: Encoder, value: OperationId) =
        encoder.encodeString(value.value.toString())
    override fun deserialize(decoder: Decoder): OperationId =
        OperationId(Uuid.parse(decoder.decodeString()))
}
