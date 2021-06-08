package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.editor.EdPlane.EdPlane
import firesword.app.editor.Editor.Editor
import firesword.app.Geometry.Vec2d
import firesword.app.TileModeSidebar.tileModeSidebar
import firesword.app.WorldViewStack.worldViewStack
import firesword.app.editor.Editor.Mode.{ObjectMode, TileMode}
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Widget, widgetList}
import firesword.frp.DynamicList
import firesword.frp.Frp
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyValue
import scalacss.DevDefaults.StyleA
import scalacss.DevDefaults._

import scala.language.postfixOps

object EditorView {
  object Styles extends StyleSheet.Inline {

    import dsl._

    val root: StyleA = style(
      width(100 %%),
      height(100 %%),

      display flex,
      flexDirection column,
    )

    val row: StyleA = style(
      height(100 %%),
      minHeight(0 px),
      display flex,
      flexDirection row,
    )
  }

  def editorView(editor: Editor): Widget = {
    import Frp.{implicitConst, implicitConstSome}
    import DynamicList.Implicits.implicitStatic

    document.body.addEventListener("keydown", (e: KeyboardEvent) => {
      import firesword.frp.Frp.implicitSome

      {
        val d = 32

        val deltaV: Option[Vec2d] = e.key match {
          case KeyValue.ArrowLeft => Vec2d(-d, 0)
          case KeyValue.ArrowUp => Vec2d(0, -d)
          case KeyValue.ArrowRight => Vec2d(d, 0)
          case KeyValue.ArrowDown => Vec2d(0, d)
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

    val objectModeButton = button("Object mode")

    objectModeButton.onPressed.listen(_ => {
      editor.enterMode(ObjectMode)
    })

    val tileModeButton = button("Tile mode")

    tileModeButton.onPressed.listen(_ => {
      editor.enterMode(TileMode)
    })

    val insertObjectButton = button("Insert object")

    insertObjectButton.onPressed.listen(_ => {
      editor.insertObject()
    })

    val deleteObjectButton = button("Delete object")

    deleteObjectButton.onPressed.listen(_ => {
      editor.deleteSelectedObject()
    })

    val worldPropertiesButton = button("World properties...")

    worldPropertiesButton.onPressed.listen(_ => {
      editor.editWorld()
    })

    val planePropertiesButton = button("Plane properties...")

    planePropertiesButton.onPressed.listen(_ => {
      editor.editActivePlane()
    })

    val planeSelect = select[EdPlane](
      editor.planes,
      editor.activePlane.sample(),
      _.name.sample(), // TODO: React
    )

    planeSelect.value.listen(p => {
      editor.selectPlane(p)
    })

    val toolBar = div(
      styleClass = MyStyles.toolBar,
      children = List(
        tileModeButton,
        objectModeButton,
        insertObjectButton,
        deleteObjectButton,
        worldPropertiesButton,
        planePropertiesButton,
        planeSelect,
      ),
    )

    val theDiv = {
      div(
        styleClass = Styles.root,
        //        styleClass = MyStyles.editorView,
        children = widgetList(
          toolBar,
          div(
            styleClass = Styles.row,
            children = widgetList(
              editor.mode.map(m =>
                Option.when(m == TileMode)(tileModeSidebar(editor)),
              ),
              worldViewStack(editor),
            ),
          )
        ),
      )
    }

    theDiv
  }
}
