package firesword.app

import firesword.app.RezIndex.RezTexture
import firesword.app.editor.Editor.Editor
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Cell.Cell
import firesword.frp.Frp.implicitConstSome
import firesword.frp.{DynamicList, Frp}
import org.scalajs.dom.raw.HTMLElement
import scalacss.DevDefaults.{StyleA, _}

import scala.language.{implicitConversions, postfixOps}


object TileModeSidebar {


  object Styles extends StyleSheet.Inline {

    import dsl._

    val root: StyleA = style(
      width(200 px),
      backgroundColor lightgrey,

      display flex,
      flexDirection row,
      flexWrap wrap,
      alignContent flexStart,
      alignItems center,

      overflowY scroll,
      resize horizontal,
    )

    val tileItem: StyleA = style(
      display grid,
    )

    val tileItemLayer: StyleA = style(
      gridColumn := "1",
      gridRow := "1",
    )

    val tileItemLayerBorder: StyleA = style(
      tileItemLayer,
//      borderColor(c"#00000030"),
      borderColor(c"#adadad99"),
      borderWidth(8 px),
      borderStyle solid,
    )
  }

  private def tileItem(
                        tileIndex: Int,
                        texture: RezTexture,
                        isSelected: Cell[Boolean],
                        onPointerDown: () => Unit,
                      ): Widget = {

    import DynamicList.Implicits.{implicitDynamicList, implicitStaticSingleton}
    import Frp.implicitMapSome

    val tileImage = new Widget(
      texture.htmlImage.cloneNode().asInstanceOf[HTMLElement],
    )

    tileImage.node.style = "display: block;"

    val root = div(
      styleClass = Styles.tileItem,
      children = List(
        div(
          styleClass = Styles.tileItemLayer,
          children = tileImage,
        ),
        div(styleClass = isSelected.map[StyleA](
          if (_) Styles.tileItemLayerBorder
          else Styles.tileItemLayer,
        )),
      ),
    )

    root.onPointerDown.listen(_ => onPointerDown())

    root
  }

  def tileModeSidebar(editor: Editor): Widget = {
    import DynamicList.Implicits.implicitStatic
    import Frp.implicitConstSome

    val rezIndex = editor.rezIndex

    val imageSet = rezIndex.getImageSet("LEVEL3_TILES_ACTION").get

    div(
      styleClass = Styles.root,
      children = imageSet.texturesByIndex.toSeq.sortBy(_._1)
        .map(p => {
          val (tileIndex, texture) = p
          tileItem(
            tileIndex,
            texture,
            isSelected = editor.selectedTile.map(_ == tileIndex),
            onPointerDown = () => {
              editor.selectTile(tileIndex)
            }
          )
        }).toList,
    )
  }
}
