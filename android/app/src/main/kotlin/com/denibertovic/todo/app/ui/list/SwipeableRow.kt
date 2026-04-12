package com.denibertovic.todo.app.ui.list

import android.view.HapticFeedbackConstants
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.spring
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.Undo
import androidx.compose.material.icons.filled.Done
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.Stable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import com.denibertovic.todo.app.ui.theme.TodoColors
import kotlinx.coroutines.launch

/**
 * Encapsulates all mutable state for the swipe-to-complete gesture.
 * Grouping it in a class removes the need for six separate `remember`
 * blocks and makes the state transitions easier to reason about.
 */
@Stable
class SwipeToActionState {
    val offsetX = Animatable(0f)
    val rowHeight = Animatable(1f)
    val rowAlpha = Animatable(1f)

    var rowWidthPx by mutableStateOf(1f)
    var hasPassedThreshold by mutableStateOf(false)
    var isEdgeSwipe by mutableStateOf(false)
    var isDismissing by mutableStateOf(false)
    var measuredHeightPx by mutableIntStateOf(0)
}

@Composable
fun rememberSwipeToActionState() = remember { SwipeToActionState() }

private const val EDGE_ZONE_FRACTION = 0.4f
private const val COMMIT_FRACTION = 0.35f

/**
 * Todoist-style edge-gated swipe to complete. The gesture is only
 * recognized when the finger starts within the left 40 % of the row.
 * Dragging past 35 % commits the action. On commit the row slides
 * off-screen, collapses, and fires [onComplete] with haptic feedback.
 */
@Composable
fun SwipeableRow(
    itemId: String,
    isCompleted: Boolean,
    onComplete: () -> Unit,
    modifier: Modifier = Modifier,
    content: @Composable () -> Unit,
) {
    val scope = rememberCoroutineScope()
    val view = LocalView.current
    view.isHapticFeedbackEnabled = true
    val state = rememberSwipeToActionState()
    val density = LocalDensity.current

    val progress = (state.offsetX.value / state.rowWidthPx).coerceIn(0f, 1f)

    val swipeColor = if (isCompleted) TodoColors.swipeUncomplete() else TodoColors.swipeComplete()
    val swipeIcon = if (isCompleted) Icons.AutoMirrored.Filled.Undo else Icons.Default.Done
    val bgColor = swipeColor.copy(alpha = progress.coerceIn(0f, 1f))
    val iconAlpha = ((progress / COMMIT_FRACTION) * 1.5f).coerceIn(0f, 1f)

    Box(
        modifier = modifier
            .fillMaxWidth()
            .then(
                if (state.measuredHeightPx > 0)
                    Modifier.height(with(density) {
                        (state.measuredHeightPx * state.rowHeight.value).toDp()
                    })
                else Modifier
            )
            .alpha(state.rowAlpha.value)
            .onSizeChanged {
                state.rowWidthPx = it.width.toFloat().coerceAtLeast(1f)
                if (state.measuredHeightPx == 0) state.measuredHeightPx = it.height
            }
    ) {
        // Background layer
        Box(
            modifier = Modifier
                .matchParentSize()
                .background(bgColor, shape = MaterialTheme.shapes.medium)
                .padding(start = 20.dp),
            contentAlignment = Alignment.CenterStart,
        ) {
            Icon(
                swipeIcon,
                contentDescription = null,
                tint = Color.White,
                modifier = Modifier.alpha(iconAlpha),
            )
        }

        // Foreground — the actual card content
        Box(
            modifier = Modifier
                .offset { IntOffset(state.offsetX.value.toInt(), 0) }
                .pointerInput(itemId, isCompleted) {
                    detectHorizontalDragGestures(
                        onDragStart = { startOffset ->
                            if (!state.isDismissing) {
                                state.isEdgeSwipe =
                                    startOffset.x <= state.rowWidthPx * EDGE_ZONE_FRACTION
                                state.hasPassedThreshold = false
                            }
                        },
                        onDragEnd = {
                            if (state.isDismissing) return@detectHorizontalDragGestures
                            if (state.isEdgeSwipe &&
                                state.offsetX.value / state.rowWidthPx >= COMMIT_FRACTION
                            ) {
                                state.isDismissing = true
                                view.performHapticFeedback(HapticFeedbackConstants.CONFIRM)
                                onComplete()
                                scope.launch {
                                    state.offsetX.animateTo(
                                        state.rowWidthPx * 1.2f,
                                        animationSpec = spring(dampingRatio = 0.8f, stiffness = 300f),
                                    )
                                    launch { state.rowAlpha.animateTo(0f) }
                                    state.rowHeight.animateTo(0f, animationSpec = tween(200))
                                    // Reset for potential recomposition with new data
                                    state.isDismissing = false
                                    state.offsetX.snapTo(0f)
                                    state.rowHeight.snapTo(1f)
                                    state.rowAlpha.snapTo(1f)
                                    state.measuredHeightPx = 0
                                }
                            } else {
                                scope.launch { state.offsetX.animateTo(0f) }
                            }
                            state.isEdgeSwipe = false
                            state.hasPassedThreshold = false
                        },
                        onDragCancel = {
                            if (!state.isDismissing) {
                                state.isEdgeSwipe = false
                                state.hasPassedThreshold = false
                                scope.launch { state.offsetX.animateTo(0f) }
                            }
                        },
                        onHorizontalDrag = { _, dragAmount ->
                            if (!state.isEdgeSwipe || state.isDismissing) {
                                return@detectHorizontalDragGestures
                            }
                            scope.launch {
                                val newValue =
                                    (state.offsetX.value + dragAmount).coerceIn(0f, state.rowWidthPx)
                                state.offsetX.snapTo(newValue)
                            }
                            val nowPast =
                                state.offsetX.value / state.rowWidthPx >= COMMIT_FRACTION
                            if (nowPast && !state.hasPassedThreshold) {
                                view.performHapticFeedback(HapticFeedbackConstants.CLOCK_TICK)
                                state.hasPassedThreshold = true
                            } else if (!nowPast && state.hasPassedThreshold) {
                                state.hasPassedThreshold = false
                            }
                        },
                    )
                }
        ) {
            content()
        }
    }
}
