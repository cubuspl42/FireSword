package firesword.app

import firesword.app.DialogContainer.dialogContainer
import firesword.app.WorldView.worldViewOuter
import firesword.app.editor.Editor.Editor
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Widget, widgetList}
import firesword.frp.Frp

import scala.language.implicitConversions

object WorldViewStack {
  def worldViewStack(editor: Editor): Widget = {
    import Frp.implicitConstSome

    div(
      styleClass = MyStyles.worldViewStack,
      children = widgetList(
        worldViewOuter(editor),
        dialogContainer(editor),
      ),
    )
  }
}
