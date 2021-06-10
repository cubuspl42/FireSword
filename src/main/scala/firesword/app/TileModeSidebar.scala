package firesword.app

import firesword.app.Json.ImageSet
import firesword.app.RezIndex.{RezImageSet, RezTexture}
import firesword.app.TileModeSidebar.Styles.selectionColor
import firesword.app.editor.Editor.Editor
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Widget, widgetList}
import firesword.frp.Cell.Cell
import firesword.frp.Frp.implicitConstSome
import firesword.frp.{DynamicList, Frp}
import org.scalajs.dom.console
import org.scalajs.dom.raw.HTMLElement
import scalacss.DevDefaults.{StyleA, _}

import scala.language.{implicitConversions, postfixOps}


object TileModeSidebar {


  object Styles extends StyleSheet.Inline {

    import dsl._

    val selectionColor = c"#adadad99"

    val root: StyleA = style(
      paddingTop(4 px),
      width(200 px),
      backgroundColor lightgrey,

      display flex,
      flexDirection column,
      alignItems center,

      gap(8 px),
      resize horizontal,
    )

    val wrap: StyleA = style(
      display flex,
      flexDirection row,
      flexWrap wrap,
      alignContent flexStart,
      alignItems center,

      overflowY scroll,
    )

    val eraserBorder: StyleA = style(
      borderColor transparent,
      borderWidth(4 px),
      borderStyle solid,
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
      borderColor(selectionColor),
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
    import Frp.{implicitConstSome, implicitConst, implicitMapSome}

    def buildWrap(imageSet: RezImageSet) = {
      div(
        styleClass = Styles.wrap,
        children = imageSet.texturesByIndex.toSeq.sortBy(_._1)
          .map(p => {
            val (tileIndex, texture) = p
            tileItem(
              tileIndex,
              texture,
              isSelected = editor.selectedTile.map(_.contains(tileIndex)),
              onPointerDown = () => {
                editor.selectTile(tileIndex)
              }
            )
          }).toList,
      )
    }

    val rezIndex = editor.rezIndex

    def eraserButtonElement() = {
      val eraserButton = button("Eraser")

      eraserButton.onPressed.listen(_ => editor.selectEraser())

      div(
        styleClass = Styles.eraserBorder,
        inlineStyle = editor.isEraserSelected.map(
          if (_) "border-color: grey;" else ""
        ),
        children = widgetList(
          eraserButton,
        )
      )
    }

    val wrap = editor.activePlane.map(plane => {
      val primaryImageSet = plane.primaryImageSet
      val imageSetOpt = rezIndex.getImageSet(s"LEVEL${editor.levelIndex}_TILES_${primaryImageSet}")
      imageSetOpt.fold(span("Error"))(buildWrap)
    })

    div(
      styleClass = Styles.root,
      children = widgetList(
        eraserButtonElement(),
        wrap,
      )
    )
  }
}
