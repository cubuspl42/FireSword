package firesword.app.utils

import firesword.scalajsdomext.HTMLImageElementExt.implicitHTMLImageElementExt
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.Future
import scala.scalajs.js.Promise

object ImageUtils {
  def loadImage(src: String): Future[HTMLImageElement] =
    new Promise[HTMLImageElement]((resolve, reject) => {
      val img = document.createElement("img").asInstanceOf[HTMLImageElement]
      img.onload = _ => resolve(img)
      img.onerror = _ => reject(new Error(s"Error while loading image ${src}"))
      img.src = src
    }).toFuture
}
