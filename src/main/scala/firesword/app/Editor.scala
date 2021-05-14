package firesword.app

import firesword.app.Camera.{Camera, FreeCamera}
import firesword.app.EdObject.EdObject
import firesword.app.TilesView.TileImageBank
import firesword.frp.Cell.Cell
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.MutCell.MutCell
import firesword.wwd.DataStream
import firesword.wwd.Wwd.readWorld
import org.scalajs.dom._
import org.scalajs.dom.experimental.Fetch.fetch
import org.scalajs.dom.experimental.Response

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

object Editor {
  case class Vec2(x: Double, y: Double) {
    def *(s: Double): Vec2 =
      Vec2(x * s, y * s)

    def /(s: Double): Vec2 =
      Vec2(x / s, y / s)

    def +(that: Vec2): Vec2 =
      Vec2(x + that.x, y + that.y)

    def -(that: Vec2): Vec2 =
      Vec2(x - that.x, y - that.y)
  }

  private val tileSize = 64

  case class TileCoord(i: Int, j: Int)

  type Tile = Int

  class Editor(
                worldBuffer: ArrayBuffer,
                val resourceBank: ResourceBank,
              ) {
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

      entries.filter(_._2 > 0).toMap
    }

    private val _hoveredTile = new MutCell[TileCoord](TileCoord(5, 8))

    private val _tiles = new MutDynamicMap(loadTiles())

    val tiles: DynamicMap[TileCoord, Tile] = _tiles

    val objects: Set[EdObject] = plane.objects.map(wObject => {
      new EdObject(
        position = Vec2(wObject.x, wObject.y),
        imageSetId = DataStream.decoder.decode(wObject.imageSet.byteArray),
      )
    }).toSet

    val hoveredTile: Cell[TileCoord] = _hoveredTile

    val _zoom = new MutCell(1.0)

    val cameraZoom: Cell[Double] = _zoom

    def zoomCamera(delta: Double): Unit = {
      _zoom.update(_ + delta)
    }

    val camera = new Camera(FreeCamera(
      initialFocusPoint = Vec2(world.startX, world.startY),
      zoom = cameraZoom,
    ))

    val cameraFocusPoint: Cell[Vec2] = camera.focusPoint

    def hoverTile(coord: TileCoord): Unit = {
      _hoveredTile.set(coord)
    }

    def hoverNextTile(): Unit = {
      val oldCoord = hoveredTile.sample()
      hoverTile(TileCoord(oldCoord.i, oldCoord.j + 1))
    }

    def insertTile(coord: TileCoord): Unit = {
      val tileId = 100
      console.log(s"Inserting $tileId @ $coord")
      _tiles.put(coord, tileId)
    }

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

  def failOnUnsuccessfulResponse(response: Response): Response = {
    if (response.ok) {
      response
    } else {
      throw new Exception("Fetch error")
    }
  }

  private def fetchWorldBuffer(): Future[ArrayBuffer] =
    for (
      response <- fetch("assets/worlds/WORLD.WWD").toFuture
        .map(failOnUnsuccessfulResponse);
      worldBuffer <- response.arrayBuffer().toFuture
    ) yield worldBuffer

  def load(): Future[Editor] = {
    for (
      worldBuffer <- fetchWorldBuffer();
      resourceBank <- ResourceBank.load()
    ) yield {
      new Editor(
        worldBuffer = worldBuffer,
        resourceBank = resourceBank,
      )
    }
  }
}
