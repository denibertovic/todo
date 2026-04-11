package com.denibertovic.todo.app.data.net

import com.denibertovic.todo.core.crdt.HealthResponse
import com.denibertovic.todo.core.crdt.RegisterRequest
import com.denibertovic.todo.core.crdt.RegisterResponse
import com.denibertovic.todo.core.crdt.SyncRequest
import com.denibertovic.todo.core.crdt.SyncResponse
import com.denibertovic.todo.core.json.TodoJson
import io.ktor.client.HttpClient
import io.ktor.client.call.body
import io.ktor.client.engine.android.Android
import io.ktor.client.plugins.HttpTimeout
import io.ktor.client.plugins.contentnegotiation.ContentNegotiation
import io.ktor.client.plugins.defaultRequest
import io.ktor.client.request.get
import io.ktor.client.request.header
import io.ktor.client.request.post
import io.ktor.client.request.setBody
import io.ktor.http.ContentType
import io.ktor.http.HttpHeaders
import io.ktor.http.contentType
import io.ktor.serialization.kotlinx.json.json

/**
 * Thin wrapper around a Ktor client for the three endpoints the
 * Android app talks to. One [SyncApi] instance per logical server:
 * building a Ktor client is cheap but not free and it owns its own
 * connection pool, so we keep a single instance alive for the life
 * of the `SyncRepository`.
 */
class SyncApi(private val baseUrl: String) {

    private val client: HttpClient = HttpClient(Android) {
        install(ContentNegotiation) {
            json(TodoJson)
        }
        install(HttpTimeout) {
            // Sync can take a while on slow links with large op pages —
            // be generous on the read timeout but strict on connect.
            connectTimeoutMillis = 15_000
            requestTimeoutMillis = 60_000
            socketTimeoutMillis = 60_000
        }
        defaultRequest {
            contentType(ContentType.Application.Json)
        }
    }

    /**
     * POST /sync with mandatory bearer auth. The auth token is sent
     * in both the header and the request body so both post- and
     * pre-hardening servers accept it.
     */
    suspend fun sync(authToken: String, request: SyncRequest): SyncResponse {
        val response = client.post("$baseUrl/sync") {
            header(HttpHeaders.Authorization, "Bearer $authToken")
            setBody(request)
        }
        return response.body()
    }

    /** POST /register using a single-use invite code. No auth header yet. */
    suspend fun register(request: RegisterRequest): RegisterResponse =
        client.post("$baseUrl/register") {
            setBody(request)
        }.body()

    /** GET /health — unauthenticated, used by the settings screen to verify the URL. */
    suspend fun health(): HealthResponse = client.get("$baseUrl/health").body()

    fun close() {
        client.close()
    }
}
