package firesword.app

import firesword.app.Editor.Vec2
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.frp.EventStream.EventStream
import firesword.frp.EventStreamSink.EventStreamSink
import firesword.frp.MutCell.MutCell

import scala.language.implicitConversions

object Camera {
  class Camera(initialState: CameraState) {
    val state: Cell[CameraState] =
      Cell.followFirst[CameraState](initialState, _.nextState)

    val focusPoint: Cell[Vec2] =
      state.switchMapC(_.focusPoint)
  }

  object CameraEquation {
    // Equation:
    // targetPoint = (worldPoint - focusPoint) * zoom

    def solveForTargetPoint(worldPoint: Vec2, focusPoint: Vec2, zoom: Double): Vec2 =
      (worldPoint - focusPoint) * zoom

    def solveForFocusPoint(worldPoint: Vec2, targetPoint: Vec2, zoom: Double): Vec2 =
      worldPoint - targetPoint / zoom

    def solveForWorldPoint(targetPoint: Vec2, focusPoint: Vec2, zoom: Double): Vec2 =
      targetPoint / zoom + focusPoint
  }

  abstract class CameraState {
    val focusPoint: Cell[Vec2]

    val nextState: EventStream[CameraState]
  }

  case class FreeCamera(initialFocusPoint: Vec2, zoom: Cell[Double]) extends CameraState {
    private val _focusPoint = new MutCell(initialFocusPoint)

    override val focusPoint: Cell[Vec2] = _focusPoint

    private val _nextState = new EventStreamSink[CameraState]()

    override val nextState: EventStream[CameraState] = _nextState

    def moveCamera(delta: Vec2): Unit = {
      _focusPoint.update(_ + delta)
    }

    def dragCamera(targetPoint: Cell[Vec2], stop: EventStream[Unit]): Unit = {
      val anchorPoint = CameraEquation.solveForWorldPoint(
        targetPoint = targetPoint.sample(),
        focusPoint = focusPoint.sample(),
        zoom = zoom.sample(),
      )

      _nextState.send(DraggedCamera(
        targetPoint = targetPoint,
        anchorPoint = anchorPoint,
        stop = stop,
        zoom = zoom,
      ))
    }
  }

  case class DraggedCamera(
                            targetPoint: Cell[Vec2],
                            anchorPoint: Vec2,
                            stop: EventStream[Unit],
                            zoom: Cell[Double]
                          ) extends CameraState {
    override val focusPoint: Cell[Vec2] = Cell.map2(
      targetPoint,
      zoom,
      (tp: Vec2, z: Double) =>
        CameraEquation.solveForFocusPoint(
          worldPoint = anchorPoint,
          targetPoint = tp,
          zoom = z,
        ),
    )

    override val nextState: EventStream[CameraState] =
      stop.map(_ => FreeCamera(
        initialFocusPoint = focusPoint.sample(),
        zoom = zoom,
      ))
  }
}
