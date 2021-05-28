package firesword.app

import firesword.app.EditObjectDialog.editObjectDialog
import firesword.app.EditPlaneDialog.editPlaneDialog
import firesword.app.editor.Editor.{EditObject, EditPlane, Editor}
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.DynamicList


object DialogContainer {

  import firesword.frp.Frp.implicitConstSome

  def dialogContainer(
                       editor: Editor
                     ): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStaticSingleton

    val dialogOpt = editor.editContext.map(editContextOpt =>
      editContextOpt.map {
        case EditObject(edObject) => editObjectDialog(editor, edObject)
        case EditPlane(edPlane) => editPlaneDialog(editor, edPlane)
      }
    )

    div(
      styleClass = MyStyles.dialogContainer,
      children = DynamicList.fuseSome(dialogOpt),
    )
  }
}
