package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.EdPlane.EdPlane
import firesword.app.Editor.Editor
import firesword.app.Geometry.Vec2d
import firesword.app.WorldViewStack.worldViewStack
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Widget, widgetList}
import firesword.frp.DynamicList
import firesword.frp.Frp
import firesword.wwd.Wwd.Plane
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyValue

import scala.language.implicitConversions

object EditorView {
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

    val planeSelect = select[EdPlane](
      editor.planes,
      _.name,
    )

    planeSelect.value.listen(p => {
      editor.selectPlane(p)
    })

    val toolBar = div(
      styleClass = MyStyles.toolBar,
      children = List(
        planeSelect
      ),
    )

    val theDiv = {
      div(
        styleClass = MyStyles.editorView,
        children = widgetList(
          toolBar,
          worldViewStack(editor),
        ),
      )
    }

    theDiv
  }
}
