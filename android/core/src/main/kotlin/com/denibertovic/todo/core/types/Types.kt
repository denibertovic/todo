package com.denibertovic.todo.core.types

import com.denibertovic.todo.core.json.ContextSerializer
import com.denibertovic.todo.core.json.LinkSerializer
import com.denibertovic.todo.core.json.PrioritySerializer
import com.denibertovic.todo.core.json.ProjectSerializer
import kotlinx.datetime.LocalDate
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonClassDiscriminator

/**
 * The todo.txt priority register, `(A)` through `(Z)`.
 *
 * Mirrors `Priority` in `src/Todo/Types.hs:251`. Serialized on the
 * wire as a single uppercase letter (`"A"`), not the bracket form —
 * the bracket form is only for rendering a [TodoItem] back to
 * `todo.txt`.
 */
@Serializable(with = PrioritySerializer::class)
enum class Priority {
    A, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z;

    /** `"(A)"` — used by the renderer when writing out a `todo.txt` line. */
    fun render(): String = "($name)"

    companion object {
        /** Parse a single-letter priority code, returning null on anything else. */
        fun fromLetter(c: Char): Priority? =
            if (c in 'A'..'Z') entries[c - 'A'] else null
    }
}

/** `+projectName` in todo.txt. Serialized as a plain string. */
@Serializable(with = ProjectSerializer::class)
@JvmInline
value class Project(val name: String) {
    fun render(): String = "+$name"
}

/** `@contextName` in todo.txt. Serialized as a plain string. */
@Serializable(with = ContextSerializer::class)
@JvmInline
value class Context(val name: String) {
    fun render(): String = "@$name"
}

/** A URL occurring inline in a todo description. */
@Serializable(with = LinkSerializer::class)
@JvmInline
value class Link(val url: String) {
    fun render(): String = url
}

/**
 * Tag — free-form `key:value` metadata attached to a todo item, plus
 * the well-known `due:*` and `origin:` forms that the CLI treats
 * specially.
 *
 * Wire shape matches `ToJSON Tag` in `src/Todo/Types.hs:352-356`:
 *
 *     {"type":"tag","key":"k","value":"v"}
 *     {"type":"due_date","date":"2026-04-20"}
 *     {"type":"due_next"}
 *     {"type":"origin","link":"https://..."}
 */
@Serializable
sealed class Tag {
    abstract fun render(): String

    @Serializable
    @SerialName("tag")
    data class KeyValue(val key: String, val value: String) : Tag() {
        override fun render(): String = "$key:$value"
    }

    @Serializable
    @SerialName("due_date")
    data class DueDate(val date: LocalDate) : Tag() {
        override fun render(): String = "due:$date"
    }

    /** `due:next` — a symbolic "next time I look at this" marker. */
    @Serializable
    @SerialName("due_next")
    data object Next : Tag() {
        override fun render(): String = "due:next"
    }

    @Serializable
    @SerialName("origin")
    data class Origin(val link: Link) : Tag() {
        override fun render(): String = "origin:${link.url}"
    }
}

/**
 * Metadata — any parsed token on a todo line that isn't the priority,
 * date, or completion marker. This splits into structured items
 * (project/context/tag) and unstructured items (string/link) that
 * become part of the description when rendering.
 *
 * Wire shape matches `ToJSON Metadata` in `src/Todo/Types.hs:368-373`.
 * Note that the `string` variant uses a `value` field, not `string`.
 */
@Serializable
sealed class Metadata {
    abstract fun render(): String

    @Serializable
    @SerialName("project")
    data class ProjectM(val project: Project) : Metadata() {
        override fun render(): String = project.render()
    }

    @Serializable
    @SerialName("context")
    data class ContextM(val context: Context) : Metadata() {
        override fun render(): String = context.render()
    }

    @Serializable
    @SerialName("tag")
    data class TagM(val tag: Tag) : Metadata() {
        override fun render(): String = tag.render()
    }

    @Serializable
    @SerialName("string")
    data class Str(@SerialName("value") val value: String) : Metadata() {
        override fun render(): String = value
    }

    @Serializable
    @SerialName("link")
    data class LinkM(val link: Link) : Metadata() {
        override fun render(): String = link.render()
    }
}

/**
 * A single todo.txt line, broken into its component pieces.
 *
 * `metadata` holds only the *structured* tokens (projects, contexts,
 * tags) — free-form words and links are collapsed into `description`
 * during parsing, matching the CLI's `incompleteTask` / `completedTask`
 * parsers in `src/Todo/Parser.hs:145-202`.
 *
 * Wire shape matches `ToJSON TodoItem` in `src/Todo/Types.hs:386-393`:
 *
 *     {
 *       "priority": "A" | null,
 *       "description": "...",
 *       "metadata": [ ... ],
 *       "created_at": "YYYY-MM-DD" | null,
 *       "completed_at": "YYYY-MM-DD" | null
 *     }
 */
@Serializable
data class TodoItem(
    val priority: Priority? = null,
    val description: String,
    val metadata: List<Metadata> = emptyList(),
    @SerialName("created_at") val createdAt: LocalDate? = null,
    @SerialName("completed_at") val completedAt: LocalDate? = null,
)

/**
 * Status wrapper around a [TodoItem] — either currently completed or
 * still incomplete. Uses a `status` discriminator on the wire, not
 * the default `type`, to match the Haskell `ToJSON (Todo a)` instance
 * at `src/Todo/Types.hs:404-406`.
 */
@OptIn(ExperimentalSerializationApi::class)
@Serializable
@JsonClassDiscriminator("status")
sealed class Todo {
    abstract val item: TodoItem

    @Serializable
    @SerialName("completed")
    data class Completed(override val item: TodoItem) : Todo()

    @Serializable
    @SerialName("incomplete")
    data class Incomplete(override val item: TodoItem) : Todo()

    val isCompleted: Boolean get() = this is Completed

    fun mapItem(f: (TodoItem) -> TodoItem): Todo = when (this) {
        is Completed -> Completed(f(item))
        is Incomplete -> Incomplete(f(item))
    }
}
