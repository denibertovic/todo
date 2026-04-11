package com.denibertovic.todo.app.data

import com.denibertovic.todo.app.data.db.DeviceStateKeys
import com.denibertovic.todo.app.data.db.TodoDatabase
import com.denibertovic.todo.app.data.net.SyncApi
import com.denibertovic.todo.core.crdt.RegisterRequest

/**
 * Handles the one-time `POST /register` handshake with a new sync
 * server. On success the returned device ID, auth token, and server
 * URL are persisted into `device_state` so every subsequent
 * [SyncRepository.sync] call can authenticate.
 *
 * The device name is cosmetic — it just shows up in
 * `todo-sync-server list-invite-codes` and server logs. Sensible
 * defaults from the user's phone model are fine.
 */
class RegistrationRepository(
    private val db: TodoDatabase,
    private val apiProvider: (baseUrl: String) -> SyncApi = { SyncApi(it) },
) {

    sealed class Result {
        data object Success : Result()
        data class Failure(val error: Throwable) : Result()
    }

    suspend fun register(serverUrl: String, inviteCode: String, deviceName: String): Result {
        val api = apiProvider(serverUrl)
        return try {
            val resp = api.register(RegisterRequest(deviceName, inviteCode))
            val dao = db.deviceStateDao()
            dao.put(DeviceStateKeys.DEVICE_ID, resp.deviceId.toString())
            dao.put(DeviceStateKeys.AUTH_TOKEN, resp.authToken)
            dao.put(DeviceStateKeys.SERVER_URL, serverUrl)
            dao.put(DeviceStateKeys.DEVICE_NAME, deviceName)
            Result.Success
        } catch (t: Throwable) {
            Result.Failure(t)
        } finally {
            api.close()
        }
    }

    /** True iff a prior registration has been persisted to device_state. */
    suspend fun isRegistered(): Boolean {
        val dao = db.deviceStateDao()
        return dao.get(DeviceStateKeys.DEVICE_ID) != null &&
            dao.get(DeviceStateKeys.AUTH_TOKEN) != null &&
            dao.get(DeviceStateKeys.SERVER_URL) != null
    }
}
