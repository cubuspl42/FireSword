package firesword.app.editor

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.Geometry.Vec2d
import firesword.app.RezIndex
import firesword.app.RezIndex.RezIndex
import firesword.app.TileImageBank.TileImageBank
import firesword.app.editor.EdObject.EdObject
import firesword.app.editor.EdPlane.EdPlane
import firesword.frp.Cell.Cell
import firesword.frp.DynamicList
import firesword.frp.DynamicList.DynamicList
import firesword.frp.MutCell.MutCell
import firesword.scalajsdomext.Fetch.fetchArrayBuffer
import firesword.wwd.DataStream.ByteString.decode
import firesword.wwd.Wwd.{World, WwdPlaneFlags, planeNameBufferSize, readWorld}
import org.scalajs.dom.window

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
    val tileImageBank = new TileImageBank(rezIndex, levelIndex = levelIndex)

    //    val imageSetBank = new ImageSetBank()

    val planes: DynamicList[EdPlane] =
      DynamicList.static(world.planes.map(p => new EdPlane(p)))

    private def _initialActivePlane() = {
      val initialPlanes = planes.content.sample()
      initialPlanes
        .find((p: EdPlane) => {
          println(s"p.wwdPlane.flags: ${p.wwdPlane.flags}")
          (p.wwdPlane.flags & WwdPlaneFlags.MAIN_PLANE) != 0
        })
        .getOrElse(initialPlanes.head)
    }

    private val _activePlane = new MutCell(_initialActivePlane())

    def activePlane: Cell[EdPlane] = _activePlane

    def selectPlane(plane: EdPlane): Unit = {
      _activePlane.sample().saveCameraFocusPoint(camera.focusPoint.sample())

      val cameraState = camera.state.sample()
      cameraState match {
        case freeCamera: FreeCamera =>
          freeCamera.focusAt(plane.savedCameraFocusPoint)
        case _ => ()
      }

      _activePlane.set(plane)
    }

    val prefixMap = Map(
      decode(world.prefix1) -> decode(world.imageSet1),
      decode(world.prefix2) -> decode(world.imageSet2),
      decode(world.prefix3) -> decode(world.imageSet3),
      decode(world.prefix4) -> decode(world.imageSet4),
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
