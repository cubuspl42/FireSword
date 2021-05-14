package firesword.app

import firesword.app.Editor.Vec2
import firesword.app.MapExt.implicitMapOpsExt
import firesword.app.TilesView.TileImageBank.loadImage
import firesword.scalajsdomext.Fetch.{fetchArrayBuffer, fetchJson}
import org.scalajs.dom.raw.HTMLImageElement

import scala.collection.MapOps
import scala.concurrent.Future
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.scalajs.js

object MapExt {
  class MapExt[K, V](self: Map[K, V]) {
    final def traverse[K2, V2](fn: (K, V) => Future[(K2, V2)]): Future[Map[K2, V2]] =
      Future.traverse[(K, V), (K2, V2), Iterable](self)({
        case (k1: K, v1: V) => fn(k1, v1)
      }).map(_.toMap)
  }

  implicit def implicitMapOpsExt[K, V](self: Map[K, V]): MapExt[K, V] =
    new MapExt(self)
}


private object Json {
  @js.native
  trait Root extends js.Object {
    // Fully-qualified image set ID to image set
    val imageSets: js.Dictionary[ImageSet] = js.native
  }

  @js.native
  trait ImageSet extends js.Object {
    // Frame index (as string) to PID filename
    val frames: js.Dictionary[String] = js.native

    // PID filename to sprite metadata
    val sprites: js.Dictionary[SpriteMetadata] = js.native
  }

  @js.native
  trait SpriteMetadata extends js.Object {
    // PID path
    val path: String = js.native

    // PID offset in form [x, y]
    val offset: js.Array[Double] = js.native
  }
}

object RezIndex {
  class RezIndex(
                  private val imageSets: Map[String, RezImageSet]
                ) {
    def getImageSet(fqImageSetId: String): Option[RezImageSet] =
      imageSets.get(fqImageSetId)
  }

  class RezImageSet(
                     private val frames: Map[Int, String],
                     private val textures: Map[String, RezTexture],
                   ) {
    def getTexture(i: Int): Option[RezTexture] =
      frames.get(i).flatMap(
        pidFilename => textures.get(pidFilename)
      )
  }

  class RezTexture(
                    val htmlImage: HTMLImageElement,
                    val offset: Vec2,
                  )


  private def loadImageSet(imageSetJson: Json.ImageSet): Future[RezImageSet] = {
    val frames = imageSetJson.frames.toMap.map({
      case (frameIndex: String, pidFilename: String) => (frameIndex.toInt, pidFilename)
    })

    for (
      textures <- imageSetJson.sprites.toMap.traverse({
        case (pidFilename: String, spriteMetadata: Json.SpriteMetadata) => {
          val offsetArray = spriteMetadata.offset
          val offset = new Vec2(offsetArray(0), offsetArray(1))

          val pidPath = spriteMetadata.path
          val pngPath = pidPath.replaceFirst(".PID", ".png")
          val fullPngPath = s"/assets/images/CLAW/$pngPath"

          loadImage(fullPngPath).map(image =>
            (
              pidFilename,
              new RezTexture(
                htmlImage = image,
                offset = offset,
              ),
            )
          )
        }
      })
    ) yield new RezImageSet(
      frames = frames,
      textures = textures,
    )
  }

  def load(): Future[RezIndex] = for {
    rezIndexJsonAny <- fetchJson("assets/rezIndex.json");
    imageSets <- {
      val rezIndexJson = rezIndexJsonAny.asInstanceOf[Json.Root]
      val imageSetsMap = rezIndexJson.imageSets.toMap.filter({
        case (fqImageSetId: String, _) => fqImageSetId.startsWith("LEVEL1_")
      })

      imageSetsMap.traverse({
        case (fqImageSetId: String, imageSetJson: Json.ImageSet) =>
          loadImageSet(imageSetJson).map(imageSet => (fqImageSetId, imageSet))
      })
    }
  } yield new RezIndex(imageSets = imageSets)
}
