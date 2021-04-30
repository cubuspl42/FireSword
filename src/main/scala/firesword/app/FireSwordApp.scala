package firesword.app

import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Frp.{Cell, MutCell}
import org.scalajs.dom.{Event, document}
import scalacss.StyleA

object FireSwordApp {
  case class TileCoord(i: Int, j: Int)

  class Editor {
    private val _hoveredTile = new MutCell[TileCoord](TileCoord(5, 8))

    val hoveredTile: Cell[TileCoord] = _hoveredTile

    def hoverTile(coord: TileCoord): Unit = {
      _hoveredTile.set(coord)
    }

    def hoverNextTile(): Unit = {
      val oldCoord = hoveredTile.sample()
      hoverTile(TileCoord(oldCoord.i, oldCoord.j + 1))
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
      checkersView(editor)
    ))
  }

  def checkersView(editor: Editor): Widget = {
    def tableCell(i: Int, j: Int, style: StyleA) = {
      val effectiveStyle = editor.hoveredTile.map(ht => Some(
        if (ht == TileCoord(i, j)) MyStyles.tdHovered
        else style
      ))

      val theDiv = div(
        style = effectiveStyle,
        List(),
      )

      theDiv.onPointerDown.listen(_ => {
        editor.hoverTile(TileCoord(i, j))
      })

      theDiv
    }

    def tableRow(i: Int, isEven: Boolean) = {
      val deltaJ = if (isEven) 1 else 0

      div(
        style = MyStyles.tr,
        (1 to 16).map(j =>
          if ((j + deltaJ) % 2 == 0)
            tableCell(i, j, MyStyles.td1)
          else
            tableCell(i, j, MyStyles.td2)
        ).toList
      )
    }

    div(
      style = MyStyles.table,
      (1 to 16).map(i =>
        tableRow(i, isEven = i % 2 == 0)
      ).toList
    )
  }
}
