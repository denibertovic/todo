package com.denibertovic.todo.core.types

import com.denibertovic.todo.core.parser.TodoParser
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import kotlinx.datetime.LocalDate

class RendererTest : DescribeSpec({

    describe("renderItem") {
        it("renders a priority + description + project") {
            val item = TodoItem(
                priority = Priority.A,
                description = "buy milk",
                metadata = listOf(Metadata.ProjectM(Project("groceries"))),
            )
            item.renderItem() shouldBe "(A) buy milk +groceries"
        }

        it("renders a creation date between priority and description") {
            val item = TodoItem(
                priority = Priority.B,
                description = "Call Mom",
                createdAt = LocalDate(2024, 3, 15),
            )
            item.renderItem() shouldBe "(B) 2024-03-15 Call Mom"
        }
    }

    describe("renderLine") {
        it("renders a completed todo in spec order: x (pri) done created desc") {
            val todo = Todo.Completed(
                TodoItem(
                    priority = Priority.A,
                    description = "Call Mom",
                    createdAt = LocalDate(2024, 3, 15),
                    completedAt = LocalDate(2024, 3, 16),
                )
            )
            todo.renderLine() shouldBe "x (A) 2024-03-16 2024-03-15 Call Mom"
        }
    }

    describe("round-trip parse . render == id") {
        val samples = listOf(
            "(A) buy milk +groceries @store",
            "(B) 2024-03-15 Call Mom",
            "x 2024-03-16 Call Mom",
            "x (A) 2024-03-16 2024-03-15 Call Mom +project @context",
            "foo bar baz +proj @ctx key:value",
            "(C) Finish report due:2024-12-31 +work",
        )

        samples.forEach { line ->
            it("round-trips: $line") {
                val parsed = TodoParser.parseLine(line).getOrThrow()
                val rendered = parsed.renderLine()
                // A second parse of the render should match the first
                // parse exactly; rendering is not necessarily
                // byte-identical (metadata may be reordered) but
                // semantically stable.
                val reparsed = TodoParser.parseLine(rendered).getOrThrow()
                reparsed shouldBe parsed
            }
        }
    }

    describe("renderFile") {
        it("newline-joins and appends a trailing newline") {
            val todos = listOf(
                Todo.Incomplete(TodoItem(description = "a")),
                Todo.Incomplete(TodoItem(description = "b")),
            )
            todos.renderFile() shouldBe "a\nb\n"
        }
    }
})
