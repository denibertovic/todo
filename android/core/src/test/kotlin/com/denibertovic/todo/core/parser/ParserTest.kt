package com.denibertovic.todo.core.parser

import com.denibertovic.todo.core.types.Context
import com.denibertovic.todo.core.types.Link
import com.denibertovic.todo.core.types.Metadata
import com.denibertovic.todo.core.types.Priority
import com.denibertovic.todo.core.types.Project
import com.denibertovic.todo.core.types.Tag
import com.denibertovic.todo.core.types.Todo
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import kotlinx.datetime.LocalDate

/**
 * Ports the fixture set from `test/Test/Parser/ParserSpec.hs`. Each
 * Kotlin case should produce the same [Todo] value the Haskell test
 * asserts for the same input string.
 */
class ParserTest : DescribeSpec({

    describe("date") {
        it("parses 2007-01-02") {
            TodoParser.tryParseDate("2007-01-02") shouldBe LocalDate(2007, 1, 2)
        }
        it("parses 2007-1-2") {
            TodoParser.tryParseDate("2007-1-2") shouldBe LocalDate(2007, 1, 2)
        }
        it("parses 07-1-2 as 2007-1-2") {
            TodoParser.tryParseDate("07-1-2") shouldBe LocalDate(2007, 1, 2)
        }
        it("rejects single-digit year") {
            TodoParser.tryParseDate("7-1-2") shouldBe null
        }
    }

    describe("classifyToken") {
        it("classifies +project") {
            TodoParser.classifyToken("+someProject1") shouldBe
                Metadata.ProjectM(Project("someProject1"))
        }
        it("classifies @context") {
            TodoParser.classifyToken("@someContext1") shouldBe
                Metadata.ContextM(Context("someContext1"))
        }
        it("classifies key:value") {
            TodoParser.classifyToken("key:value") shouldBe
                Metadata.TagM(Tag.KeyValue("key", "value"))
        }
        it("classifies due:next") {
            TodoParser.classifyToken("due:next") shouldBe Metadata.TagM(Tag.Next)
        }
        it("classifies due:2019-01-19") {
            TodoParser.classifyToken("due:2019-01-19") shouldBe
                Metadata.TagM(Tag.DueDate(LocalDate(2019, 1, 19)))
        }
        it("classifies plain words") {
            TodoParser.classifyToken("foo") shouldBe Metadata.Str("foo")
        }
        it("classifies https url as Link") {
            TodoParser.classifyToken("https://example.com/x") shouldBe
                Metadata.LinkM(Link("https://example.com/x"))
        }
    }

    describe("parseLine") {
        it("parses a plain incomplete todo") {
            val t = TodoParser.parseLine("+project @context foo bar @context2 +project2")
                .getOrThrow()
            t.shouldBe(
                Todo.Incomplete(
                    com.denibertovic.todo.core.types.TodoItem(
                        priority = null,
                        description = "foo bar",
                        metadata = listOf(
                            Metadata.ProjectM(Project("project")),
                            Metadata.ContextM(Context("context")),
                            Metadata.ContextM(Context("context2")),
                            Metadata.ProjectM(Project("project2")),
                        ),
                        createdAt = null,
                        completedAt = null,
                    ),
                )
            )
        }

        it("parses an incomplete todo with priority") {
            val t = TodoParser.parseLine("(A) +project @context foo bar")
                .getOrThrow()
            t.shouldBe(
                Todo.Incomplete(
                    com.denibertovic.todo.core.types.TodoItem(
                        priority = Priority.A,
                        description = "foo bar",
                        metadata = listOf(
                            Metadata.ProjectM(Project("project")),
                            Metadata.ContextM(Context("context")),
                        ),
                    ),
                )
            )
        }

        it("parses an incomplete todo with creation date") {
            val t = TodoParser.parseLine("(A) 2024-03-15 Call Mom").getOrThrow()
            t.shouldBe(
                Todo.Incomplete(
                    com.denibertovic.todo.core.types.TodoItem(
                        priority = Priority.A,
                        description = "Call Mom",
                        metadata = emptyList(),
                        createdAt = LocalDate(2024, 3, 15),
                    ),
                )
            )
        }

        it("parses a completed task with completion date only") {
            val t = TodoParser.parseLine("x 2024-03-16 Call Mom").getOrThrow()
            t.shouldBe(
                Todo.Completed(
                    com.denibertovic.todo.core.types.TodoItem(
                        description = "Call Mom",
                        completedAt = LocalDate(2024, 3, 16),
                    ),
                )
            )
        }

        it("parses a completed task with both dates") {
            val t = TodoParser.parseLine("x 2024-03-16 2024-03-15 Call Mom").getOrThrow()
            t.shouldBe(
                Todo.Completed(
                    com.denibertovic.todo.core.types.TodoItem(
                        description = "Call Mom",
                        createdAt = LocalDate(2024, 3, 15),
                        completedAt = LocalDate(2024, 3, 16),
                    ),
                )
            )
        }

        it("parses a completed task with priority, both dates, project, context") {
            val t = TodoParser
                .parseLine("x (A) 2024-03-16 2024-03-15 Call Mom +project @context")
                .getOrThrow()
            t.shouldBe(
                Todo.Completed(
                    com.denibertovic.todo.core.types.TodoItem(
                        priority = Priority.A,
                        description = "Call Mom",
                        metadata = listOf(
                            Metadata.ProjectM(Project("project")),
                            Metadata.ContextM(Context("context")),
                        ),
                        createdAt = LocalDate(2024, 3, 15),
                        completedAt = LocalDate(2024, 3, 16),
                    ),
                )
            )
        }

        it("parses due:2019-01-19 at the end of a line") {
            val t = TodoParser
                .parseLine("(A) +project @context foo bar due:2019-01-19")
                .getOrThrow()
            t shouldBe Todo.Incomplete(
                com.denibertovic.todo.core.types.TodoItem(
                    priority = Priority.A,
                    description = "foo bar",
                    metadata = listOf(
                        Metadata.ProjectM(Project("project")),
                        Metadata.ContextM(Context("context")),
                        Metadata.TagM(Tag.DueDate(LocalDate(2019, 1, 19))),
                    ),
                )
            )
        }
    }
})
