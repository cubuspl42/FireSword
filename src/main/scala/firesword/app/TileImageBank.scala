package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.editor.EdObject.EdObject
import firesword.app.editor.Editor.Editor
import firesword.app.Geometry.Vec2d
import firesword.app.MapExt.implicitMapOpsExt
import firesword.app.RezIndex.{RezIndex, RezTexture}
import firesword.dom.Dom.Tag.div
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.scalajsdomext.HTMLImageElementExt.implicitHTMLImageElementExt
import firesword.wwd.Wwd.DrawFlags
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.scalajs.js.Promise

object TileImageBank {
  class TileImageBank(
                       rezIndex: RezIndex,
                       levelIndex: Int,
                     ) {
    def getTileImage(
                      imageSetSuffix: String,
                      tileIndex: Int,
                    ): HTMLImageElement = {
      val imageSet = rezIndex.getImageSet(s"LEVEL${levelIndex}_TILES_${imageSetSuffix}").get
      val rezTexture: RezTexture = imageSet.getTexture(tileIndex).getOrElse(imageSet.getTexture(-1).get)
      rezTexture.htmlImage
    }
  }


}
