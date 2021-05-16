package firesword.app

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.EdObject.EdObject
import firesword.app.Geometry.Vec2d
import firesword.app.RezIndex.RezIndex
import firesword.app.TilesView.TileImageBank
import firesword.frp.Cell.Cell
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.DynamicSet
import firesword.frp.DynamicSet.{DynamicSet, MutDynamicSet}
import firesword.frp.MutCell.MutCell
import firesword.scalajsdomext.Fetch.fetchArrayBuffer
import firesword.wwd.DataStream
import firesword.wwd.Wwd.{readWorld, rezFileLength}
import org.scalajs.dom._
import org.scalajs.dom.experimental.Fetch.fetch
import org.scalajs.dom.experimental.Response

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

object Editor {


  private val tileSize = 64

  case class TileCoord(i: Int, j: Int)

  type Tile = Int

  class Editor(
                worldBuffer: ArrayBuffer,
                val resourceBank: ResourceBank,
                val rezIndex: RezIndex,
              ) {

    def findClosestObject(p: Vec2d): EdObject =
      objects.content.sample().minBy(obj => (obj.position.sample() - p).length)

    val tileImageBank: TileImageBank = resourceBank.tileImageBank

    val imageSetBank = new ImageSetBank()

    private val world = readWorld(worldBuffer)

    private val plane = world.planes(1)

    private def loadTiles(): Map[TileCoord, Int] = {

      val entries = for (
        i <- (0 until plane.tilesHigh);
        j <- (0 until plane.tilesWide)
      ) yield {
        val k = i * plane.tilesWide + j
        val tile = plane.tiles(k)
        TileCoord(i, j) -> tile
      }

      entries

        .filter(_._2 > 0)
//        .take(100)


        .toMap
    }

    private val _tiles = new MutDynamicMap(loadTiles())

    val tiles: DynamicMap[TileCoord, Tile] = _tiles

    val objects: DynamicSet[EdObject] = DynamicSet.of(
      plane.objects
//        .take(500)
        . map(wwdObject => {
        new EdObject(
          wwdObject = wwdObject,
          initialPosition = Vec2d(wwdObject.x, wwdObject.y),
          imageSetId = DataStream.decoder.decode(wwdObject.imageSet.byteArray),
        )
      }).toSet
    )


    private val _initialSelectedObject = objects.content.sample().filter(o => o.wwdObject.id == 3024).head

    private val _selectedObject = new MutCell[Option[EdObject]](Some(_initialSelectedObject))

    def selectClosestObject(wp: Vec2d): Unit ={
      _selectedObject.set(Some(findClosestObject(wp)))
    }

    def selectedObject: Cell[Option[EdObject]] = _selectedObject

    val _zoom = new MutCell(1.0)

    val cameraZoom: Cell[Double] = _zoom

    def zoomCamera(delta: Double): Unit = {
      _zoom.update(_ + delta)
    }

    val camera = new Camera(FreeCamera(
      initialFocusPoint = Vec2d(world.startX, world.startY),
      zoom = cameraZoom,
    ))

    val cameraFocusPoint: Cell[Vec2d] = camera.focusPoint

    // FIXME
    cameraFocusPoint.listen(_ => {})

    def getTileCoordAtPoint(x: Double, y: Double): TileCoord =
      TileCoord(
        (y / tileSize).toInt,
        (x / tileSize).toInt,
      )
  }

  def delay(milliseconds: Int): Future[Unit] = {
    val p = Promise[Unit]()
    js.timers.setTimeout(milliseconds) {
      p.success(())
    }
    p.future
  }

  private def fetchWorldBuffer(): Future[ArrayBuffer] =
    fetchArrayBuffer("assets/worlds/WORLD.WWD")

  def load(): Future[Editor] = {
    for (
      worldBuffer <- fetchWorldBuffer();
      resourceBank <- ResourceBank.load();
      rezIndex <- RezIndex.load()
    ) yield {
      println("new Editor")
      new Editor(
        worldBuffer = worldBuffer,
        resourceBank = resourceBank,
        rezIndex = rezIndex,
      )
    }
  }
}
