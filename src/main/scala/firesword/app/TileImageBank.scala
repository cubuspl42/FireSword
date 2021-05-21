package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.EdObject.EdObject
import firesword.app.Editor.Editor
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
    def getTileImage(tileIndex: Int): HTMLImageElement = {
      val imageSet = rezIndex.getImageSet(s"LEVEL${levelIndex}_TILES_ACTION").get
      val rezTexture: RezTexture = imageSet.getTexture(tileIndex).getOrElse(imageSet.getTexture(-1).get)
      rezTexture.htmlImage
    }
  }

  object TileImageBank {
    private val tileIndexToPidFilename = Map(
      -1 -> "012.PID",
      12 -> "012.PID",
      74 -> "74.PID",
      302 -> "302.PID",
      303 -> "303.PID",
      304 -> "304.PID",
      305 -> "305.PID",
      307 -> "307.PID",
      308 -> "308.PID",
      309 -> "309.PID",
      310 -> "310.PID",
      311 -> "311.PID",
      312 -> "312.PID",
      313 -> "313.PID",
      319 -> "319.PID",
      320 -> "320.PID",
      321 -> "321.PID",
      322 -> "322.PID",
      323 -> "323.PID",
      326 -> "326.PID",
      327 -> "327.PID",
      328 -> "328.PID",
      331 -> "331.PID",
      332 -> "332.PID",
      334 -> "334.PID",
      401 -> "401.PID",
      402 -> "402.PID",
      403 -> "403.PID",
      404 -> "404.PID",
      405 -> "405.PID",
      406 -> "406.PID",
      407 -> "407.PID",
      408 -> "408.PID",
      920 -> "920.PID",
      922 -> "922.PID",
      925 -> "925.PID",
      926 -> "926.PID",
      933 -> "933.PID",
      934 -> "934.PID",
      935 -> "935.PID",
      937 -> "937.PID",
      938 -> "938.PID"
    )

    def loadImage(src: String): Future[HTMLImageElement] =
      new Promise[HTMLImageElement]((resolve, reject) => {
        val img = document.createElement("img").asInstanceOf[HTMLImageElement]
        img.onload = _ => resolve(img)
        img.onerror = _ => reject(new Error(s"Error while loading image ${src}"))
        img.src = src
      }).toFuture


  }


}
