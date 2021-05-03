package firesword.app

import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.frp.DynamicList.implicitDynamicList
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.EventStream.EventStream
import firesword.frp.EventStreamSink.EventStreamSink
import firesword.frp.Frp.{Const, implicitConst, implicitConstSome}
import firesword.frp.MutCell.MutCell
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyValue
import scalacss.StyleA

import scala.language.implicitConversions

object EditorView {
  //  implicit class AsInstanceOfOption[T](val self: T) {
  //    def asOptionOf[T1 <: T]: Option[T1] =
  //      self match {
  //        case t: T1 => Some(t)
  //        case _ => None
  //      }
  //  }

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

  class Camera(initialState: CameraState) {
    //    val state: Cell[CameraState] = initialState.asCell

    val state: Cell[CameraState] =
      Cell.followFirst[CameraState](initialState, _.nextState)

    val focusPoint: Cell[Vec2] =
      state.switchMapC(_.focusPoint)
  }

  object CameraEquation {
    // Equation:
    // targetPoint = (worldPoint - focusPoint) * zoom

    def solveForTargetPoint(worldPoint: Vec2, focusPoint: Vec2, zoom: Double): Vec2 =
      (worldPoint - focusPoint) * zoom

    def solveForFocusPoint(worldPoint: Vec2, targetPoint: Vec2, zoom: Double): Vec2 =
      worldPoint - targetPoint / zoom

    def solveForWorldPoint(targetPoint: Vec2, focusPoint: Vec2, zoom: Double): Vec2 =
      targetPoint / zoom + focusPoint
  }

  abstract class CameraState {
    val focusPoint: Cell[Vec2]

    val nextState: EventStream[CameraState]

    //    def asCell: Cell[CameraState] =
    //      Cell.switchHoldC(this, nextState.map(_.asCell))
  }

  case class FreeCamera(initialFocusPoint: Vec2, zoom: Cell[Double]) extends CameraState {
    private val _focusPoint = new MutCell(initialFocusPoint)

    override val focusPoint: Cell[Vec2] = _focusPoint

    private val _nextState = new EventStreamSink[CameraState]()

    override val nextState: EventStream[CameraState] = _nextState

    def moveCamera(delta: Vec2): Unit = {
      _focusPoint.update(_ + delta)
    }

    def dragCamera(targetPoint: Cell[Vec2], stop: EventStream[Unit]): Unit = {
      val anchorPoint = CameraEquation.solveForWorldPoint(
        targetPoint = targetPoint.sample(),
        focusPoint = focusPoint.sample(),
        zoom = zoom.sample(),
      )

      _nextState.send(DraggedCamera(
        targetPoint = targetPoint,
        anchorPoint = anchorPoint,
        stop = stop,
        zoom = zoom,
      ))
    }
  }

  case class DraggedCamera(
                            targetPoint: Cell[Vec2],
                            anchorPoint: Vec2,
                            stop: EventStream[Unit],
                            zoom: Cell[Double]
                          ) extends CameraState {
    override val focusPoint: Cell[Vec2] = Cell.map2(
      targetPoint,
      zoom,
      (tp: Vec2, z: Double) =>
        CameraEquation.solveForFocusPoint(
          worldPoint = anchorPoint,
          targetPoint = tp,
          zoom = z,
        ),
    )

    override val nextState: EventStream[CameraState] =
      stop.map(_ => FreeCamera(
        initialFocusPoint = focusPoint.sample(),
        zoom = zoom,
      ))
  }


  private val tileSize = 64

  case class TileCoord(i: Int, j: Int)

  type Tile = Int

  class Editor {

    private val _hoveredTile = new MutCell[TileCoord](TileCoord(5, 8))

    private val _tiles = new MutDynamicMap(Map(
      TileCoord(0, 0) -> 1,
      TileCoord(0, 1) -> 2,
      TileCoord(1, 0) -> 3,
      TileCoord(1, 1) -> 4,
    ))

    val tiles: DynamicMap[TileCoord, Tile] = _tiles

    val hoveredTile: Cell[TileCoord] = _hoveredTile

    val _zoom = new MutCell(1.0)

    val cameraZoom: Cell[Double] = _zoom

    def zoomCamera(delta: Double): Unit = {
      _zoom.update(_ + delta)
    }

    val camera = new Camera(FreeCamera(
      initialFocusPoint = Vec2(0.0, 0.0),
      zoom = cameraZoom,
    ))

    //    val _cameraPosition = new MutCell(Vec2(0.0, 0.0))

    val cameraFocusPoint: Cell[Vec2] = camera.focusPoint


    //    def moveCamera(delta: Vec2): Unit = {
    //      val oldPosition = _cameraPosition.sample()
    //      _cameraPosition.set(oldPosition + delta)
    //    }

    //    def dragCamera(targetPosition: Cell[Vec2], stop: EventStream[Unit]): Unit = {
    //
    //    }

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

  def editorView(): Widget = {
    val editor = new Editor()

    document.body.addEventListener("keydown", (e: KeyboardEvent) => {
      import firesword.frp.Frp.implicitSome

      {
        val d = 32

        val deltaV: Option[Vec2] = e.key match {
          case KeyValue.ArrowLeft => Vec2(-d, 0)
          case KeyValue.ArrowUp => Vec2(0, -d)
          case KeyValue.ArrowRight => Vec2(d, 0)
          case KeyValue.ArrowDown => Vec2(0, d)
          case _ => None
        }

        deltaV.foreach(dv => {
          val cameraState = editor.camera.state.sample()
          cameraState match {
            case freeCamera: FreeCamera =>
              freeCamera.moveCamera(dv)
            case _ => ()
          }
        })
      }

      {
        val deltaZ: Option[Double] = e.key match {
          case "+" => 0.1
          case "-" => -0.1
          case _ => None
        }

        deltaZ.foreach(editor.zoomCamera)
      }
    })

    val theDiv = div(
      styleClass = MyStyles.editorView,
      children = List(
        span(editor.hoveredTile.map(_.toString)),
        tilesView(editor),
      ),
    )

    theDiv
  }

  def tilesView(editor: Editor): Widget = {
    def tileFragment(coord: TileCoord, tile: Tile) = {
      val left = coord.j * tileSize
      val top = coord.i * tileSize

      val styleClass =
        if ((coord.j + (coord.i % 2)) % 2 == 0) MyStyles.tileFragment1
        else MyStyles.tileFragment2

      div(
        styleClass = styleClass,
        inlineStyle = s"left: ${left}px; top: ${top}px;",
        children = List(
          p(s"$tile")
        )
      )
    }

    val inlineStyle = Cell.map2(
      editor.cameraFocusPoint,
      editor.cameraZoom,
      (fp: Vec2, z: Double) =>
        s"transform-origin: top left; transform: scale($z) translate(${-fp.x}px, ${-fp.y}px);"
    )

    val tilesRootDiv = div(
      styleClass = MyStyles.tilesRoot,
      inlineStyle = inlineStyle,
      children = editor.tiles.toList.map { case (coord, tile) =>
        tileFragment(coord, tile)
      },
    )

    val tilesOriginDiv = div(
      styleClass = MyStyles.tilesOrigin,
      children = List(tilesRootDiv),
    )

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(tilesOriginDiv)
    )

    def calculateTargetPoint(e: PointerEvent): Vec2 = {
      val rect = tilesOriginDiv.node.getBoundingClientRect()
      val x = e.clientX - rect.left
      val y = e.clientY - rect.top

      Vec2(x, y)
    }

    tilesViewDiv.onPointerDown.listen(e => {
      val cameraState = editor.camera.state.sample()

      cameraState match {
        case freeCamera: FreeCamera =>
          val targetPoint = tilesViewDiv.onPointerMove.hold(e)
            .map(calculateTargetPoint)

          freeCamera.dragCamera(
            targetPoint = targetPoint,
            stop = tilesViewDiv.onPointerUp.map(_ => ()),
          )
      }


      //      val rect = tilesRootDiv.node.getBoundingClientRect()
      //      val x = e.clientX - rect.left
      //      val y = e.clientY - rect.top
      //
      //      console.log(s"Pointer event @ $x, $y")
      //      editor.insertTile(editor.getTileCoordAtPoint(x, y))
    })

    tilesViewDiv
  }

  def checkersView(editor: Editor): Widget = {
    def tableCell(i: Int, j: Int, style: StyleA) = {
      val effectiveStyle = editor.hoveredTile.map(ht => Some(
        if (ht == TileCoord(i, j)) MyStyles.tdHovered
        else style
      ))

      val theDiv = div(
        List(),
        styleClass = effectiveStyle,
        inlineStyle = Const(""),
      )

      theDiv.onPointerDown.listen(_ => {
        editor.hoverTile(TileCoord(i, j))
      })

      theDiv
    }

    def tableRow(i: Int, isEven: Boolean) = {
      val deltaJ = if (isEven) 1 else 0

      div(
        styleClass = MyStyles.tr,
        children = (1 to 16).map(j =>
          if ((j + deltaJ) % 2 == 0)
            tableCell(i, j, MyStyles.td1)
          else
            tableCell(i, j, MyStyles.td2)
        ).toList
      )
    }

    implicitConst()

    div(
      styleClass = MyStyles.table,
      children = (1 to 16).map(i =>
        tableRow(i, isEven = i % 2 == 0)
      ).toList
    )
  }
}
