package firesword.app

import firesword.app.Editor.{Editor, FreeCamera, Vec2}
import firesword.app.TilesView.TileImageBank.loadImage
import firesword.app.TilesView.{TileImageBank, tilesView, tilesViewOuter}
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.frp.DynamicMap.{DynamicMap, MutDynamicMap}
import firesword.frp.EventStream.EventStream
import firesword.frp.EventStreamSink.EventStreamSink
import firesword.frp.Frp.{implicitConst, implicitConstSome}
import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.readWorld
import org.scalajs.dom._
import org.scalajs.dom.experimental.Fetch.fetch
import org.scalajs.dom.experimental.Response
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

object EditorView {
  def editorView(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic

    document.body.addEventListener("keydown", (e: KeyboardEvent) => {
      import firesword.frp.Frp.implicitSome

      {
        val d = 32

        val deltaV: Option[Vec2] = e.key match {
          case KeyValue.ArrowLeft => Vec2(-d, 0)
          case KeyValue.ArrowUp => Vec2(0, -d)
          case KeyValue.ArrowRight => Vec2(d, 0)
          case KeyValue.ArrowDown => Vec2(0, d)
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


    val theDiv = div(
      styleClass = MyStyles.editorView,
      children = List(
        span(editor.hoveredTile.map(_.toString)),
        tilesViewOuter(editor),
      ),
    )

    theDiv
  }
}
