package firesword.app

import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.DynamicList.implicitDynamicList
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.Frp.{Cell, Const, MutCell, implicitConst, implicitConstSome}
import org.scalajs.dom.{Event, document}
import scalacss.StyleA

object FireSwordApp {
  case class TileCoord(i: Int, j: Int)

  type Tile = Int

  class Editor {
    private val _hoveredTile = new MutCell[TileCoord](TileCoord(5, 8))

    private val _tiles = new MutDynamicMap[TileCoord, Tile](Map(
      TileCoord(0, 0) -> 1,
      TileCoord(0, 1) -> 2,
      TileCoord(1, 0) -> 3,
      TileCoord(1, 1) -> 4,
    ))

    val tiles: DynamicMap[TileCoord, Tile] = _tiles

    val hoveredTile: Cell[TileCoord] = _hoveredTile

    def hoverTile(coord: TileCoord): Unit = {
      _hoveredTile.set(coord)
    }

    def hoverNextTile(): Unit = {
      val oldCoord = hoveredTile.sample()
      hoverTile(TileCoord(oldCoord.i, oldCoord.j + 1))
    }

    def insertTile(): Unit = {
      _tiles.put(TileCoord(5, 5), 100)
    }
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

    div(List(
      p(editor.hoveredTile.map(_.toString)),
      tilesView(editor),
    ))
  }

  def tilesView(editor: Editor): Widget = {
    def tileFragment(coord: TileCoord, tile: Tile) = {
      val left = coord.j * 64
      val top = coord.i * 64

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

    val theDiv = div(
      styleClass = MyStyles.tilesView,
      children = editor.tiles.toList.map { case (coord, tile) =>
        tileFragment(coord, tile)
      },
    )

    theDiv.onPointerDown.listen(_ => {
      editor.insertTile()
    })

    theDiv
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
