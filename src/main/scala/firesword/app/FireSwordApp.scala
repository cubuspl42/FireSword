package firesword.app

import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.DynamicList.implicitDynamicList
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.Frp.{Cell, Const, EventStream, EventStreamSink, MutCell, implicitConst, implicitConstSome}
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.{Event, KeyboardEvent, console, document}
import scalacss.StyleA

import scala.language.implicitConversions

object FireSwordApp {
  case class Vec2(x: Double, y: Double) {
    def +(that: Vec2): Vec2 =
      Vec2(x + that.x, y + that.y)
  }

  class Camera(initialState: CameraState) {
    val state: Cell[CameraState] = initialState.nextState.hold(initialState)

    val focusPoint: Cell[Vec2] = state.switchMapC(_.focusPoint)
  }

  abstract class CameraState {
    val focusPoint: Cell[Vec2]

    val nextState: EventStream[CameraState]
  }

  class FreeCamera(initialFocusPoint: Vec2) extends CameraState {
    private val _focusPoint = new MutCell(initialFocusPoint)

    override val focusPoint: Cell[Vec2] = _focusPoint

    private val _nextState = new EventStreamSink[CameraState]()

    override val nextState: EventStream[CameraState] = _nextState


    def moveCamera(delta: Vec2): Unit = {
      _focusPoint.update(_ + delta)
    }

    def dragCamera(targetPoint: Cell[Vec2], stop: EventStream[Unit]): Unit = {
      _nextState.send(new DraggedCamera(
        targetPoint=targetPoint,
        anchorPoint = ???,
        stop = stop,
      ))
    }
  }

  class DraggedCamera(
                       targetPoint: Cell[Vec2],
                       anchorPoint: Vec2,
                       stop: EventStream[Unit],
                     ) extends CameraState {

    override val focusPoint: Cell[Vec2] = ???

    override val nextState: EventStream[CameraState] = ???
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

    val zoom: Cell[Double] = _zoom

    def zoomCamera(delta: Double): Unit = {
      val oldZoom = _zoom.sample()
      _zoom.set(oldZoom + delta)
    }

    val _cameraPosition = new MutCell(Vec2(0.0, 0.0))

    val cameraPosition: Cell[Vec2] = _cameraPosition


    def moveCamera(delta: Vec2): Unit = {
      val oldPosition = _cameraPosition.sample()
      _cameraPosition.set(oldPosition + delta)
    }

    def dragCamera(targetPosition: Cell[Vec2], stop: EventStream[Unit]): Unit = {

    }

    def hoverTile(coord: TileCoord): Unit = {
      _hoveredTile.set(coord)
    }

    def hoverNextTile(): Unit = {
      val oldCoord = hoveredTile.sample()
      hoverTile(TileCoord(oldCoord.i, oldCoord.j + 1))
    }

    def insertTile(coord: TileCoord): Unit = {
      val tileId = 100
      console.log(s"Inserting ${tileId} @ ${coord}")
      _tiles.put(coord, tileId)
    }

    def getTileCoordAtPoint(x: Double, y: Double) =
      TileCoord(
        (y / tileSize).toInt,
        (x / tileSize).toInt,
      )
  }

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (_: Event) => {
      render()
    })
  }

  private def renderCss(): Unit = {
    import scalacss.DevDefaults._

    val style = document.createElement("style")
    style.setAttribute("type", "text/css")
    style.appendChild(document.createTextNode(MyStyles.render))

    document.head.appendChild(style)
  }

  def render(): Unit = {
    renderCss()

    val root = document.getElementById("root")
    Dom.render(root, rootView())
  }

  def rootView(): Widget = {
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

        deltaV.foreach(editor.moveCamera)
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
      styleClass = MyStyles.root,
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
          p(s"${tile}")
        )
      )
    }


    val inlineStyle = editor.cameraPosition.map(p =>
      s"transform-origin: top left; transform: scale(1.5) translate(${-p.x}px, ${-p.y}px);"
    )


    val tilesRootDiv = div(
      styleClass = MyStyles.tilesRoot,
      inlineStyle = inlineStyle,
      children = editor.tiles.toList.map { case (coord, tile) =>
        tileFragment(coord, tile)
      },
    )

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(tilesRootDiv)
    )

    tilesViewDiv.onPointerDown.listen(e => {
      val rect = tilesRootDiv.node.getBoundingClientRect()
      val x = e.clientX - rect.left
      val y = e.clientY - rect.top

      console.log(s"Pointer event @ $x, $y")
      editor.insertTile(editor.getTileCoordAtPoint(x, y))
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
