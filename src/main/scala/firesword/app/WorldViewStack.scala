package firesword.app

import firesword.app.EditObjectDialog.editObjectDialog
import firesword.app.editor.Editor.Editor
import firesword.app.WorldView.worldViewOuter
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Widget, widgetList}
import firesword.frp.Frp

import scala.language.implicitConversions

object WorldViewStack {
  def worldViewStack(editor: Editor): Widget = {
    import Frp.implicitConstSome

    val editObjectDialogOpt = editor.editedObject.map(editedObjectOpt =>
      editedObjectOpt.map(editObjectDialog(editor, _))
    )

    div(
      styleClass = MyStyles.worldViewStack,
      children = widgetList(
        worldViewOuter(editor),
        editObjectDialogOpt,
      ),
    )
  }
}
