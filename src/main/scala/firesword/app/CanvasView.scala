package firesword.app

import firesword.dom.Dom.Widget
import firesword.frp.Cell.Cell
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas

import scala.language.implicitConversions

object CanvasView {
  def canvasView(drawFn: Cell[CanvasRenderingContext2D => Unit]): Widget = {
    val canvas = document.createElement("canvas").asInstanceOf[Canvas]

    canvas.addEventListener("contextmenu", (e: Event) => e.preventDefault())

    canvas.style = "width: 100%; height: 100%;"

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    var isDirty = true

    drawFn.listen(_ => {
      isDirty = true
    })

    var i = 0

    def requestDraw(): Unit = {
      //      i += 1
      //      if (i > 1024) return

      window.requestAnimationFrame(t => {
        val rect = canvas.getBoundingClientRect()

        val w = rect.width.toInt
        val h = rect.height.toInt

        if (canvas.width != w || canvas.height != h) {
          canvas.width = w
          canvas.height = h
        }

        // Force redrawing
//        isDirty = true

        if (isDirty) {

          val draw = drawFn.sample()

          ctx.clearRect(0, 0, canvas.width, canvas.height)

          ctx.save()
          draw(ctx)
          ctx.restore()

          isDirty = false
        }

        requestDraw()
      })
    }

    requestDraw()

    new Widget(canvas)
  }
}
