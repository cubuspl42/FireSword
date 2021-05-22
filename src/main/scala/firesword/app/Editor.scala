package firesword.app

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.EdObject.EdObject
import firesword.app.Geometry.Vec2d
import firesword.app.RezIndex.RezIndex
import firesword.app.TileImageBank.TileImageBank
import firesword.app.utils.IntMatrixMap
import firesword.base.TextDecoder.decoder
import firesword.frp.Cell.Cell
import firesword.frp.DynamicList.DynamicList
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.{DynamicList, DynamicSet}
import firesword.frp.DynamicSet.DynamicSet
import firesword.frp.MutCell.MutCell
import firesword.scalajsdomext.Fetch.fetchArrayBuffer
import firesword.wwd.{DataStream, Wwd}
import firesword.wwd.DataStream.ByteString
import firesword.wwd.DataStream.ByteString.decode
import firesword.wwd.Wwd.{World, readWorld}
import org.scalajs.dom.{console, window}

import scala.collection.mutable
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
                private val world: World,
                val rezIndex: RezIndex,
                levelIndex: Int,
              ) {

    def findClosestObject(p: Vec2d): EdObject =
      objects.content.sample().minBy(obj => (obj.position.sample() - p).length)

    val tileImageBank = new TileImageBank(rezIndex, levelIndex = levelIndex)

    //    val imageSetBank = new ImageSetBank()

    private val plane = world.planes(1)

    val planes: DynamicList[Wwd.Plane] =
      DynamicList.static(world.planes)

    val prefixMap = Map(
      decode(world.prefix1) -> decode(world.imageSet1),
      decode(world.prefix2) -> decode(world.imageSet2),
      decode(world.prefix3) -> decode(world.imageSet3),
      decode(world.prefix4) -> decode(world.imageSet4),
    )

    val tiles = new IntMatrixMap(
      width = plane.tilesWide,
      height = plane.tilesHigh,
      array = plane.tiles,
    )

    println(s"Object count: ${plane.objects.size}")

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

    private val _selectedObject = new MutCell[Option[EdObject]](None)

    def selectObject(edObject: EdObject): Unit = {
      val t1 = window.performance.now()
      _selectedObject.set(Some(edObject))
      val t2 = window.performance.now()
      //      console.log(s"t2 - t1 = ${t2 - t1}")
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

  private val levelIndexRegex = """(\d+)""".r

  def findLevelIndex(name: String): Int = {
    val levelIndexStr = levelIndexRegex findFirstIn name
    levelIndexStr.fold(
      throw new IllegalArgumentException("Level index not present in world name")
    )(_.toInt)
  }

  def load(): Future[Editor] = {
    for (
      worldBuffer <- fetchWorldBuffer();
      editor <- {
        val world = readWorld(worldBuffer)
        val levelIndex = findLevelIndex(world.name.decode())
        for (
          rezIndex <- RezIndex.load(levelIndex = levelIndex)
        ) yield new Editor(
          world = world,
          rezIndex = rezIndex,
          levelIndex = levelIndex,
        )
      }
    ) yield editor
  }
}
