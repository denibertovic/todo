package com.denibertovic.todo.core.parser

import com.denibertovic.todo.core.types.Context
import com.denibertovic.todo.core.types.Link
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.Project
import com.denibertovic.todo.core.types.Tag
import com.denibertovic.todo.core.types.Todo
import com.denibertovic.todo.core.types.TodoItem
import kotlinx.datetime.LocalDate

/**
 * Hand-written todo.txt single-line parser. The grammar is small
 * enough that using a parser combinator library would be overkill.
 *
 * Direct port of the Parsec-based parser in `src/Todo/Parser.hs`.
 * Differences from the Haskell version:
 *
 * - Tokenization is whitespace-based rather than character-by-character.
 *   The todo.txt grammar doesn't care about specific whitespace so
 *   `split(\s+)` is sufficient.
 * - Errors are returned as [Result.failure] rather than via a monad.
 *
 * Only single-line parsing is exposed here — multi-line file parsing
 * is not needed on Android because all state flows through the op
 * log rather than a persistent `todo.txt` file.
 */
object TodoParser {

    /**
     * Parse a single todo.txt line. Returns a [Result.success]
     * wrapping the [Todo] or a [Result.failure] with the parse
     * error.
     */
    fun parseLine(input: String): Result<Todo> {
        val trimmed = input.trim()
        if (trimmed.isEmpty()) {
            return Result.failure(IllegalArgumentException("Empty todo line"))
        }

        val rawTokens = trimmed.split(whitespace).toMutableList()
        if (rawTokens.isEmpty()) {
            return Result.failure(IllegalArgumentException("Empty todo line"))
        }

        // A line beginning with `x ` (lowercase x followed by space)
        // is a completed task. Per the todo.txt spec the `x` must be
        // followed by *something* — a lone "x" on a line is not a
        // completed task, so require at least one more token.
        val isCompleted = rawTokens.size > 1 && rawTokens[0] == "x"
        if (isCompleted) rawTokens.removeAt(0)

        val priority = tryConsumePriority(rawTokens)
        val firstDate = tryConsumeDate(rawTokens)
        val secondDate = tryConsumeDate(rawTokens)

        // Per `Parser.hs:184-191`: for a completed task the two dates
        // are (completionDate, creationDate) in that order, matching
        // the todo.txt spec. For an incomplete task the two dates are
        // (createdAt, completedAt) — the second slot is a quirk of
        // the original parser (it uses `optionMaybe date` twice).
        val createdAt: LocalDate?
        val completedAt: LocalDate?
        if (isCompleted) {
            completedAt = firstDate
            createdAt = secondDate
        } else {
            createdAt = firstDate
            completedAt = secondDate
        }

        if (rawTokens.isEmpty()) {
            return Result.failure(IllegalArgumentException("Todo line has no description"))
        }

        val classified = rawTokens.map(::classifyToken)

        // The description is built from the string + link tokens, in
        // order. Everything else (project/context/tag) goes into the
        // structured metadata list. This mirrors the `filter` +
        // `intercalate " "` split in `incompleteTask`/`completedTask`.
        val descriptionParts = classified.mapNotNull { md ->
            when (md) {
                is Metadata.Str -> md.value
                is Metadata.LinkM -> md.link.url
                else -> null
            }
        }
        val structured = classified.filter { md ->
            md !is Metadata.Str && md !is Metadata.LinkM
        }

        val item = TodoItem(
            priority = priority,
            description = descriptionParts.joinToString(" "),
            metadata = structured,
            createdAt = createdAt,
            completedAt = completedAt,
        )
        return Result.success(if (isCompleted) Todo.Completed(item) else Todo.Incomplete(item))
    }

    /**
     * Classify a single whitespace-separated token into a [Metadata]
     * variant. Order matters: `due:*` and `origin:` are recognized
     * before the generic `key:value` rule, URLs are recognized before
     * anything else containing a colon.
     */
    internal fun classifyToken(token: String): Metadata = when {
        token.startsWith("+") && token.length > 1 ->
            Metadata.ProjectM(Project(token.drop(1)))

        token.startsWith("@") && token.length > 1 ->
            Metadata.ContextM(Context(token.drop(1)))

        token.startsWith("https:") || token.startsWith("http:") ->
            Metadata.LinkM(Link(token))

        token == "due:next" ->
            Metadata.TagM(Tag.Next)

        token.startsWith("due:") -> {
            val rest = token.substring(4)
            val date = tryParseDate(rest)
            if (date != null) Metadata.TagM(Tag.DueDate(date))
            // If it's not a valid date fall through to a generic
            // `due:xxx` key/value tag so we don't lose information
            // from lines the CLI would otherwise reject.
            else Metadata.TagM(Tag.KeyValue("due", rest))
        }

        token.startsWith("origin:") -> {
            val rest = token.substring(7)
            Metadata.TagM(Tag.Origin(Link(rest)))
        }

        token.contains(':') -> {
            val idx = token.indexOf(':')
            Metadata.TagM(Tag.KeyValue(token.substring(0, idx), token.substring(idx + 1)))
        }

        else -> Metadata.Str(token)
    }

    /**
     * Peek the first token; if it is a parenthesized uppercase letter
     * like `(A)`, consume it and return the matching [Priority].
     * Otherwise leave the list alone and return `null`.
     */
    private fun tryConsumePriority(tokens: MutableList<String>): Priority? {
        val head = tokens.firstOrNull() ?: return null
        val pri = parsePriorityToken(head) ?: return null
        tokens.removeAt(0)
        return pri
    }

    private fun parsePriorityToken(token: String): Priority? {
        if (token.length != 3) return null
        if (token[0] != '(' || token[2] != ')') return null
        return Priority.fromLetter(token[1])
    }

    /**
     * Peek the first token; if it looks like a date, consume and
     * return it. Otherwise leave the list alone and return `null`.
     */
    private fun tryConsumeDate(tokens: MutableList<String>): LocalDate? {
        val head = tokens.firstOrNull() ?: return null
        val date = tryParseDate(head) ?: return null
        tokens.removeAt(0)
        return date
    }

    /**
     * Parse a todo.txt style date: `YYYY-MM-DD`, `YYYY-M-DD`,
     * `YYYY-MM-D`, `YYYY-M-D`, or `YY-M-D` (2-digit year → 2000+yy).
     * A single-digit year is rejected to match
     * `src/Todo/Parser.hs:34-55`.
     */
    internal fun tryParseDate(s: String): LocalDate? {
        val parts = s.split('-')
        if (parts.size != 3) return null
        val yearPart = parts[0]
        val monthPart = parts[1]
        val dayPart = parts[2]
        if (yearPart.length !in 2..4) return null
        if (monthPart.length !in 1..2) return null
        if (dayPart.length !in 1..2) return null
        if (!yearPart.all { it.isDigit() }) return null
        if (!monthPart.all { it.isDigit() }) return null
        if (!dayPart.all { it.isDigit() }) return null
        val year = yearPart.toInt().let { if (it < 100) it + 2000 else it }
        val month = monthPart.toInt()
        val day = dayPart.toInt()
        return try {
            LocalDate(year, month, day)
        } catch (_: IllegalArgumentException) {
            null
        }
    }

    private val whitespace = Regex("\\s+")
}
