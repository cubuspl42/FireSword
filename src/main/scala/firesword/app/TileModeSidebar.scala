package firesword.app

import firesword.app.editor.Editor.Editor
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
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
      flexDirection column,
      alignItems center,
      overflowY scroll,
    )
  }


  def tileModeSidebar(editor: Editor): Widget = {
    import DynamicList.Implicits.implicitStatic
    import Frp.implicitConstSome

    val rezIndex = editor.rezIndex

    val imageSet = rezIndex.getImageSet("LEVEL3_TILES_ACTION").get
    val texture = imageSet.getTexture(-1).get

    div(
      styleClass = Styles.root,
      children = (0 until 100).map(_ =>
        new Widget(texture.htmlImage.cloneNode().asInstanceOf[HTMLElement])
      ).toList,
    )
  }
}
