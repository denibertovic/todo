package com.denibertovic.todo.core.types

/**
 * Render a [TodoItem] as an incomplete todo.txt line. This is the
 * "raw" form without the leading `x ` completion marker; use
 * [Todo.renderLine] to get the full form for a [Todo] wrapper.
 *
 * Mirrors the `Show TodoItem` instance at `src/Todo/Types.hs:268-273`.
 */
fun TodoItem.renderItem(): String = buildString {
    priority?.let { append(it.render()); append(' ') }
    createdAt?.let { append(it.toString()); append(' ') }
    append(description)
    for (m in metadata) {
        append(' ')
        append(m.render())
    }
}

/**
 * Render a [Todo] as a single todo.txt line.
 *
 * Completed form mirrors `Show (Todo TodoItem)` at
 * `src/Todo/Types.hs:305-312`:
 *
 *     x (A) <completedAt> <createdAt> <description> <metadata...>
 *
 * Incomplete form falls through to [renderItem].
 */
fun Todo.renderLine(): String = when (this) {
    is Todo.Incomplete -> item.renderItem()
    is Todo.Completed -> buildString {
        append("x ")
        item.priority?.let { append(it.render()); append(' ') }
        item.completedAt?.let { append(it.toString()); append(' ') }
        item.createdAt?.let { append(it.toString()); append(' ') }
        append(item.description)
        for (m in item.metadata) {
            append(' ')
            append(m.render())
        }
    }
}

/**
 * Render a list of [Todo]s as the full contents of a `todo.txt` file.
 * Lines are newline-joined, with a trailing newline — matching
 * `Show [Todo TodoItem]` at `src/Todo/Types.hs:314-315`.
 *
 * This is the exact format the Android app uses when the user taps
 * "Export todo.txt" in Settings: the output can be piped straight
 * into the user's laptop after a disaster-recovery pull.
 */
fun List<Todo>.renderFile(): String =
    joinToString(separator = "\n", postfix = "\n") { it.renderLine() }
