package firesword.app

import firesword.app.Geometry.Vec2d
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

    val focusPoint: Cell[Vec2d] =
      state.switchMapC(_.focusPoint)
  }

  object CameraEquation {
    // Equation:
    // targetPoint = (worldPoint - focusPoint) * zoom

    def solveForTargetPoint(worldPoint: Vec2d, focusPoint: Vec2d, zoom: Double): Vec2d =
      (worldPoint - focusPoint) * zoom

    def solveForFocusPoint(worldPoint: Vec2d, targetPoint: Vec2d, zoom: Double): Vec2d =
      worldPoint - targetPoint / zoom

    def solveForWorldPoint(targetPoint: Vec2d, focusPoint: Vec2d, zoom: Double): Vec2d =
      targetPoint / zoom + focusPoint
  }

  abstract class CameraState {
    val focusPoint: Cell[Vec2d]

    val nextState: EventStream[CameraState]
  }

  case class FreeCamera(initialFocusPoint: Vec2d, zoom: Cell[Double]) extends CameraState {
    private val _focusPoint = new MutCell(initialFocusPoint)

    override val focusPoint: Cell[Vec2d] = _focusPoint

    private val _nextState = new EventStreamSink[CameraState]()

    override val nextState: EventStream[CameraState] = _nextState

    def focusAt(fp: Vec2d) = {
      _focusPoint.set(fp)
    }

    def moveCamera(delta: Vec2d): Unit = {
      _focusPoint.update(_ + delta)
    }

    def dragCamera(targetPoint: Cell[Vec2d], stop: EventStream[Unit]): Unit = {
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
                            targetPoint: Cell[Vec2d],
                            anchorPoint: Vec2d,
                            stop: EventStream[Unit],
                            zoom: Cell[Double]
                          ) extends CameraState {
    override val focusPoint: Cell[Vec2d] = Cell.map2(
      targetPoint,
      zoom,
      (tp: Vec2d, z: Double) =>
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
