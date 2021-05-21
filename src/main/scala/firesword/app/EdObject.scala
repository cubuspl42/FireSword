package firesword.app

import firesword.app.Geometry.Vec2d
import firesword.base.TextDecoder.decoder
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.frp.EventStream.EventStream
import firesword.frp.EventStreamSink.EventStreamSink
import firesword.frp.Frp.Const
import firesword.frp.MutCell.MutCell
import firesword.wwd.DataStream.ByteString
import firesword.wwd.Geometry.Rectangle
import firesword.wwd.Wwd.Object_

import scala.language.implicitConversions

object EdObject {

  private trait PositionState {
    def position: Cell[Vec2d]

    def nextState: EventStream[PositionState]
  }

  private case class IdlePosition(
                                   initialPosition: Vec2d,
                                   override val nextState: EventStream[PositionState],
                                 ) extends PositionState {
    override val position = new MutCell[Vec2d](initialPosition)
  }

  private case class MovedPosition(
                                    initialPosition: Vec2d,
                                    delta: Cell[Vec2d],
                                    override val nextState: EventStream[PositionState],
                                  ) extends PositionState {
    override val position: Cell[Vec2d] = delta.map(initialPosition + _)
  }

  class EdObject(
                  val wwdObject: Object_,
                  initialPosition: Vec2d,
                ) {
    private def decode(b: ByteString): String =
      decoder.decode(b.byteArray)

    val id = new MutCell[Int](wwdObject.id)

    val name = new MutCell(decode(wwdObject.name))
    val logic = new MutCell(decode(wwdObject.logic))
    val imageSet = new MutCell(decode(wwdObject.imageSet))
    val animation = new MutCell(decode(wwdObject.animation))

    lazy val x: Cell[Int] = position.map(_.x.toInt)
    lazy val y: Cell[Int] = position.map(_.y.toInt)

    private def setPosition(p: Vec2d): Unit = {
      _positionState.sample() match {
        case ip: IdlePosition => ip.position.set(p)
        case _ => ()
      }
    }

    def setX(x: Int): Unit = {
      setPosition(Vec2d(x.toDouble, y.sample()))
    }

    def setY(y: Int): Unit = {
      setPosition(Vec2d(x.sample(), y.toDouble))
    }

    val z = new MutCell(wwdObject.z)
    val i = new MutCell(wwdObject.i)
    val addFlags = new MutCell(wwdObject.addFlags)
    val dynamicFlags = new MutCell(wwdObject.dynamicFlags)
    val drawFlags = new MutCell(wwdObject.drawFlags)
    val userFlags = new MutCell(wwdObject.userFlags)
    val score = new MutCell(wwdObject.score)
    val points = new MutCell(wwdObject.points)
    val powerUp = new MutCell(wwdObject.powerUp)
    val damage = new MutCell(wwdObject.damage)
    val smarts = new MutCell(wwdObject.smarts)
    val health = new MutCell(wwdObject.health)

    val userValue1 = new MutCell(wwdObject.userValue1)
    val userValue2 = new MutCell(wwdObject.userValue2)
    val userValue3 = new MutCell(wwdObject.userValue3)
    val userValue4 = new MutCell(wwdObject.userValue4)
    val userValue5 = new MutCell(wwdObject.userValue5)
    val userValue6 = new MutCell(wwdObject.userValue6)
    val userValue7 = new MutCell(wwdObject.userValue7)
    val userValue8 = new MutCell(wwdObject.userValue8)
    val xMin = new MutCell(wwdObject.xMin)
    val yMin = new MutCell(wwdObject.yMin)
    val xMax = new MutCell(wwdObject.xMax)
    val yMax = new MutCell(wwdObject.yMax)
    val speedX = new MutCell(wwdObject.speedX)
    val speedY = new MutCell(wwdObject.speedY)
    val xTweak = new MutCell(wwdObject.xTweak)
    val yTweak = new MutCell(wwdObject.yTweak)
    val counter = new MutCell(wwdObject.counter)
    val speed = new MutCell(wwdObject.speed)
    val width = new MutCell(wwdObject.width)
    val height = new MutCell(wwdObject.height)
    val direction = new MutCell(wwdObject.direction)
    val faceDir = new MutCell(wwdObject.faceDir)
    val timeDelay = new MutCell(wwdObject.timeDelay)
    val frameDelay = new MutCell(wwdObject.frameDelay)
    val objectType = new MutCell(wwdObject.objectType)
    val hitTypeFlags = new MutCell(wwdObject.hitTypeFlags)
    val xMoveRes = new MutCell(wwdObject.xMoveRes)
    val yMoveRes = new MutCell(wwdObject.yMoveRes)

    //val moveRect: Rectangle= new MutCell(0)
    //val hitRect: Rectangle= new MutCell(0)
    //val attackRect: Rectangle= new MutCell(0)
    //val clipRect: Rectangle= new MutCell(0)
    //val userRect1: Rectangle= new MutCell(0)
    //val userRect2: Rectangle= new MutCell(0)


    private val _move = new EventStreamSink[MovedPosition]()

    private def _buildIdlePositionState(p: Vec2d) =
      IdlePosition(
        p,
        nextState = _move,
      )

    private val _positionState =
      Cell.followFirst[PositionState](
        _buildIdlePositionState(initialPosition),
        _.nextState,
      )

    def position: Cell[Vec2d] = _positionState.switchMapC(_.position)

    position.listen(_ => {})

    def move(delta: Cell[Vec2d], commit: EventStream[Unit]): Unit = {
      val initialPosition = position.sample()
      _move.send(MovedPosition(
        initialPosition = initialPosition,
        delta = delta,
        nextState = commit.map(_ => _buildIdlePositionState(position.sample())),
      ))
    }
  }
}
