package firesword.app

import firesword.app.TilesView.TileImageBank
import firesword.app.TilesView.TileImageBank.loadImage
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.Future

class ImageSetBank() {
  def getImage(imageSetId: String, i: Int): HTMLImageElement = {
    val img = document.createElement("img").asInstanceOf[HTMLImageElement]
    val imageSetName = imageSetId.replaceFirst("LEVEL_", "")
    img.src = s"/assets/images/CLAW/LEVEL1/IMAGES/${imageSetName}/FRAME001.png"
    img
  }
}

object ImageSetBank {

}
