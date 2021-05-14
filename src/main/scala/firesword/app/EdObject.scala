package firesword.app

import firesword.app.Editor.Vec2
import firesword.wwd.Wwd.Object_

import scala.language.implicitConversions

object EdObject {
  class EdObject(
                  val wwdObject: Object_,
                  val position: Vec2,
                  val imageSetId: String,
                )
}
