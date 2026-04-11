package com.denibertovic.todo.core.crdt

import com.denibertovic.todo.core.types.DeviceId
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * Request body for `POST /sync`. The auth token is also sent in the
 * `Authorization` header; the server prefers the header but falls
 * back to this field for older clients. New code should always rely
 * on the header and leave this `null`.
 *
 * Matches `SyncRequest` at `src/Todo/Sync/Types.hs:326-331`.
 */
@Serializable
data class SyncRequest(
    @SerialName("device_id") val deviceId: DeviceId,
    @SerialName("cursor") val cursor: SyncCursor? = null,
    @SerialName("operations") val operations: List<Operation>,
    @SerialName("auth_token") val authToken: String? = null,
)

/**
 * Response body for `POST /sync`. When `hasMore` is true the client
 * must immediately re-issue `/sync` with the returned [cursor] and
 * an empty `operations` list.
 */
@Serializable
data class SyncResponse(
    @SerialName("operations") val operations: List<Operation>,
    @SerialName("cursor") val cursor: SyncCursor? = null,
    @SerialName("has_more") val hasMore: Boolean,
)

/** Registration request using a server-issued single-use invite code. */
@Serializable
data class RegisterRequest(
    @SerialName("device_name") val deviceName: String,
    @SerialName("invite_code") val inviteCode: String,
)

/** Registration response. The returned auth token authenticates every future sync. */
@Serializable
data class RegisterResponse(
    @SerialName("device_id") val deviceId: DeviceId,
    @SerialName("auth_token") val authToken: String,
)

/** Response body for the unauthenticated `GET /health` endpoint. */
@Serializable
data class HealthResponse(
    @SerialName("status") val status: String,
)
