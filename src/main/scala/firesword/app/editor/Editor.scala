package firesword.app.editor

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.Geometry.Vec2d
import firesword.app.RezIndex
import firesword.app.RezIndex.RezIndex
import firesword.app.TileImageBank.TileImageBank
import firesword.app.editor.EdObject.EdObject
import firesword.app.editor.EdPlane.EdPlane
import firesword.base.{File, FileOptions}
import firesword.dom.Dom.Tag.anchor
import firesword.frp.Cell.Cell
import firesword.frp.DynamicList
import firesword.frp.DynamicList.DynamicList
import firesword.frp.MutCell.MutCell
import firesword.scalajsdomext.Fetch.fetchArrayBuffer
import firesword.wwd.DumpWwd.dumpWwd
import firesword.wwd.OutputDataStream.OutputStream
import firesword.wwd.Wwd.{Object_, World, WwdPlaneFlags, readWorld}
import org.scalajs.dom.{URL, console, window}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

object Editor {
  sealed trait Mode

  object ObjectMode extends Mode

  case class TileMode() extends Mode {
    val _hoverCoord = new MutCell[Option[TileCoord]](None)

    val hoverCoord = new MutCell[Option[TileCoord]](None)


    def hover(coord: TileCoord): Unit = {
      _hoverCoord.set(Some(coord))
    }
  }

  sealed trait Brush

  case class TileBrush(tile: Tile) extends Brush

  case object Eraser extends Brush

  sealed trait EditContext

  case class EditWorld(worldProperties: WorldProperties) extends EditContext

  case class EditObject(edObject: EdObject) extends EditContext

  case class EditPlane(edPlane: EdPlane) extends EditContext

  class DrawContext {

  }

  private val tileSize = 64

  case class TileCoord(i: Int, j: Int)

  type Tile = Int

  class Editor(
                private val world: World,
                val rezIndex: RezIndex,
                val levelIndex: Int,
              ) {

    val tileImageBank = new TileImageBank(rezIndex, levelIndex = levelIndex)

    val _mode = new MutCell[Mode](ObjectMode)

    def mode: Cell[Mode] = _mode

    def enterMode(newMode: Mode): Unit = {
      _mode.set(newMode)
    }

    val worldProperties = new WorldProperties(world)

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

    def prefixMap = Map(
      worldProperties.prefix1.sample() -> worldProperties.imageSet1.sample(),
      worldProperties.prefix2.sample() -> worldProperties.imageSet2.sample(),
      worldProperties.prefix3.sample() -> worldProperties.imageSet3.sample(),
      worldProperties.prefix4.sample() -> worldProperties.imageSet4.sample(),
    )

    private val _selectedObject = new MutCell[Option[EdObject]](None)

    def selectObject(edObject: EdObject): Unit = {
      val t1 = window.performance.now()
      _selectedObject.set(Some(edObject))
      val t2 = window.performance.now()
      //      console.log(s"t2 - t1 = ${t2 - t1}")
    }

    def selectedObject: Cell[Option[EdObject]] = _selectedObject

    def deleteSelectedObject(): Unit = {
      val currentSelectedObjectOpt = selectedObject.sample()
      val currentActivePlane = activePlane.sample()

      currentSelectedObjectOpt.foreach(currentSelectedObject => {
        currentActivePlane.deleteObject(currentSelectedObject)
      })
    }

    def insertObject(): Unit = {
      val currentActivePlane = activePlane.sample()
      val currentCameraFocusPoint = cameraFocusPoint.sample()

      currentActivePlane.addObject(new EdObject(
        Object_.empty,
        initialPosition = currentCameraFocusPoint,
      ))
    }

    private val _editContext = new MutCell[Option[EditContext]](None)

    def editContext: Cell[Option[EditContext]] = _editContext

    def editedObject: Cell[Option[EdObject]] = editContext.map(_.flatMap({
      case EditObject(edObject) => Some(edObject)
      case _ => None
    }))

    def editObject(edObject: EdObject): Unit = {
      _editContext.set(Some(EditObject(edObject)))
    }

    def editSelectedObject(): Unit = {
      selectedObject.sample().foreach(editObject)
    }

    def editWorld(): Unit = {
      _editContext.set(Some(EditWorld(worldProperties)))
    }

    def editActivePlane(): Unit = {
      val currentActivePlane = activePlane.sample()
      _editContext.set(Some(EditPlane(currentActivePlane)))
    }

    def stopEditing(): Unit = {
      _editContext.set(None)
    }

    private val _selectedBrush = new MutCell[Brush](Eraser)

    def selectedBrush: Cell[Brush] = _selectedBrush

    def selectedTile: Cell[Option[Tile]] = selectedBrush.map {
      case TileBrush(tile) => Some(tile)
      case _ => None
    }

    def isEraserSelected: Cell[Boolean] = selectedBrush.map {
      case Eraser => true
      case _ => false
    }

    def selectTile(tile: Tile): Unit = {
      _selectedBrush.set(TileBrush(tile))
    }

    def selectEraser(): Unit = {
      _selectedBrush.set(Eraser)
    }

    def drawAt(twp: Vec2d): Unit = {
      val currentActivePlane = activePlane.sample()
      val tileCoord = getTileCoordAtPoint(twp)
      val brush = selectedBrush.sample()

      brush match {
        case TileBrush(tile) => currentActivePlane.setTile(tileCoord, tile)
        case Eraser => currentActivePlane.setTile(tileCoord, -1)
      }
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

    def getTileCoordAtPoint(p: Vec2d): TileCoord =
      TileCoord(
        (p.y / tileSize).toInt,
        (p.x / tileSize).toInt,
      )

    def getTileCoordAtPoint(x: Double, y: Double): TileCoord =
      getTileCoordAtPoint(Vec2d(x, y))

    def save(): Unit = {
      val fileName = "test.wwd"

      val outputStream = new OutputStream()
      dumpWwd(outputStream, world)
      val worldBuffer = outputStream.toArrayBuffer()

      val file = new File(
        bits = js.Array(worldBuffer),
        name = fileName,
        options = FileOptions(
          mimeType = "application/x-wwd",
        ),
      )

      val saveAnchor = anchor(
        href = URL.createObjectURL(file),
        download = fileName,
        text = "",
      )

      saveAnchor.node.click()
    }
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
      editor <- loadBuffer(worldBuffer)
    ) yield editor
  }

  def printBufferStats(buffer: ArrayBuffer): Unit = {
    console.log(s"Length: ${buffer.byteLength}")
  }

  def loadBuffer(worldBuffer: ArrayBuffer): Future[Editor] = {
    printBufferStats(worldBuffer)

    val world = readWorld(worldBuffer)

    val outputStream = new OutputStream()
    dumpWwd(outputStream, world)

    printBufferStats(outputStream.toArrayBuffer())

    val levelIndex = findLevelIndex(world.name.decode())
    for (
      rezIndex <- RezIndex.load(levelIndex = levelIndex)
    ) yield new Editor(
      world = world,
      rezIndex = rezIndex,
      levelIndex = levelIndex,
    )
  }
}
