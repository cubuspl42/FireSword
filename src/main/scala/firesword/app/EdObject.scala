package firesword.app

import firesword.app.Geometry.Vec2d
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.frp.EventStream.EventStream
import firesword.frp.EventStreamSink.EventStreamSink
import firesword.frp.Frp.Const
import firesword.wwd.Wwd.Object_

import scala.language.implicitConversions

object EdObject {

  private case class PositionState(
                                    position: Cell[Vec2d],
                                    nextState: EventStream[PositionState],
                                  )

  class EdObject(
                  val wwdObject: Object_,
                  initialPosition: Vec2d,
                  val imageSetId: String,
                ) {
    val z: Cell[Double] = Const(1.0)

    private val _move = new EventStreamSink[PositionState]()

    private def _buildIdlePositionState(p: Vec2d) =
      PositionState(
        position = Const(p),
        nextState = _move,
      )

    private val _positionState =
      Cell.followFirst[PositionState](
        _buildIdlePositionState(initialPosition),
        _.nextState,
      )

    def position: Cell[Vec2d] = _positionState.switchMapC(_.position)

    def move(delta: Cell[Vec2d], commit: EventStream[Unit]): Unit = {
      val initialPosition = position.sample()
      _move.send(PositionState(
        position = delta.map(initialPosition + _),
        nextState = commit.map(_ => _buildIdlePositionState(position.sample())),
      ))
    }
  }
}
