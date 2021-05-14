package firesword.app

import firesword.app.Editor.Vec2
import firesword.frp.Cell.Cell
import firesword.frp.Frp.Const
import firesword.frp.MutCell
import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.Object_

import scala.language.implicitConversions

object EdObject {

  class EdObject(
                  val wwdObject: Object_,
                  initialPosition: Vec2,
                  val imageSetId: String,
                ) {
    val z: Cell[Double] = Const(1.0)

    private val _position = new MutCell(initialPosition)

    def position: Cell[Vec2] = _position;

    def move(delta: Vec2): Unit = {
      _position.update(_ + delta)
    }
  }
}
