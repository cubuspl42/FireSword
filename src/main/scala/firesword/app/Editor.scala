package firesword.app

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.EdObject.EdObject
import firesword.app.Geometry.Vec2d
import firesword.app.RezIndex.RezIndex
import firesword.app.TileImageBank.TileImageBank
import firesword.base.TextDecoder.decoder
import firesword.frp.Cell.Cell
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.DynamicSet
import firesword.frp.DynamicSet.DynamicSet
import firesword.frp.MutCell.MutCell
import firesword.scalajsdomext.Fetch.fetchArrayBuffer
import firesword.wwd.DataStream
import firesword.wwd.DataStream.ByteString
import firesword.wwd.Wwd.readWorld
import org.scalajs.dom.{console, window}

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

    //    val imageSetBank = new ImageSetBank()

    private val world = readWorld(worldBuffer)

    private val plane = world.planes(1)

    private def decode(b: ByteString): String =
      decoder.decode(b.byteArray)

    val prefixMap = Map(
      decode(world.prefix1) -> decode(world.imageSet1),
      decode(world.prefix2) -> decode(world.imageSet2),
      decode(world.prefix3) -> decode(world.imageSet3),
      decode(world.prefix4) -> decode(world.imageSet4),
    )

    println(prefixMap)

    private def loadTiles(): Map[TileCoord, Int] = {
      val tilesHigh = plane.tilesHigh
      //      val tilesWide = plane.tilesWide
      val tilesWide = 16

      val entries = for (
        i <- (0 until tilesHigh);
        j <- (0 until tilesWide)
      ) yield {
        val k = i * plane.tilesWide + j
        val tile = plane.tiles(k)
        TileCoord(i, j) -> tile
      }

      entries

        .filter(_._2 > 0)
        .toMap
    }

    private val _tiles = new MutDynamicMap(loadTiles())

    val tiles: DynamicMap[TileCoord, Tile] = _tiles

    val objects: DynamicSet[EdObject] = DynamicSet.of(
      plane.objects
        //        .take(500)
        .map(wwdObject => {
          new EdObject(
            wwdObject = wwdObject,
            initialPosition = Vec2d(wwdObject.x, wwdObject.y),
          )
        }).toSet
    )


    private val _initialSelectedObject = objects.content.sample().filter(o => o.wwdObject.id == 3024).head


    val anotherObject = objects.content.sample().filter(o => o.wwdObject.id == 1171).head

    private val _selectedObject = new MutCell[Option[EdObject]](Some(_initialSelectedObject))

    def selectObject(edObject: EdObject): Unit = {
      val t1 = window.performance.now()
      _selectedObject.set(Some(edObject))
      val t2 = window.performance.now()

      console.log(s"t2 - t1 = ${t2 - t1}")
    }

    def selectedObject: Cell[Option[EdObject]] = _selectedObject

    private val _editedObject = new MutCell[Option[EdObject]](None)

    def editedObject: Cell[Option[EdObject]] = _editedObject

    def editObject(edObject: EdObject): Unit = {
      _editedObject.set(Some(edObject))
    }

    def stopEditObject(): Unit = {
      _editedObject.set(None)
    }

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
