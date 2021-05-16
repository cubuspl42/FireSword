package firesword.app

import firesword.app.TileImageBank.TileImageBank
import firesword.app.TileImageBank.TileImageBank.loadImage
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ResourceBank(
                    val tileImageBank: TileImageBank,
                    val objectPlaceholderImage: HTMLImageElement,
                  )

object ResourceBank {
  def load(): Future[ResourceBank] =
    for (
      t <- TileImageBank.load();
      objectPlaceholderImage <- loadImage("/assets/images/CLAW/GAME/IMAGES/POWERUPS/EXTRALIFE/FRAME001.png")
    ) yield new ResourceBank(
      tileImageBank = t,
      objectPlaceholderImage = objectPlaceholderImage,
    )
}

