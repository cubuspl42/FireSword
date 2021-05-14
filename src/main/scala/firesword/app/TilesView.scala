package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.Editor.{Editor, Vec2}
import firesword.dom.Dom.Tag.div
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import firesword.frp.Cell.Cell
import firesword.scalajsdomext.HTMLImageElementExt.implicitHTMLImageElementExt
import firesword.wwd.Wwd.DrawFlags
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.scalajs.js.Promise

object TilesView {
  class TileImageBank(
                       tileIndexToPngImage: Map[Int, HTMLImageElement]
                     ) {
    def getTileImage(tileIndex: Int): HTMLImageElement =
      tileIndexToPngImage(tileIndex)
  }

  object TileImageBank {
    private val tileIndexToPidFilename = Map(
      -1 -> "012.PID",
      12 -> "012.PID",
      74 -> "74.PID",
      302 -> "302.PID",
      303 -> "303.PID",
      304 -> "304.PID",
      305 -> "305.PID",
      307 -> "307.PID",
      308 -> "308.PID",
      309 -> "309.PID",
      310 -> "310.PID",
      311 -> "311.PID",
      312 -> "312.PID",
      313 -> "313.PID",
      319 -> "319.PID",
      320 -> "320.PID",
      321 -> "321.PID",
      322 -> "322.PID",
      323 -> "323.PID",
      326 -> "326.PID",
      327 -> "327.PID",
      328 -> "328.PID",
      331 -> "331.PID",
      332 -> "332.PID",
      334 -> "334.PID",
      401 -> "401.PID",
      402 -> "402.PID",
      403 -> "403.PID",
      404 -> "404.PID",
      405 -> "405.PID",
      406 -> "406.PID",
      407 -> "407.PID",
      408 -> "408.PID",
      920 -> "920.PID",
      922 -> "922.PID",
      925 -> "925.PID",
      926 -> "926.PID",
      933 -> "933.PID",
      934 -> "934.PID",
      935 -> "935.PID",
      937 -> "937.PID",
      938 -> "938.PID"
    )

    def loadImage(src: String): Future[HTMLImageElement] =
      new Promise[HTMLImageElement]((resolve, reject) => {
        val img = document.createElement("img").asInstanceOf[HTMLImageElement]
        img.onload = _ => resolve(img)
        img.onerror = _ => reject(new Error(s"Error while loading image ${src}"))
        img.src = src
      }).toFuture

    final def traverseMap[K1, V1, K2, V2](in: Map[K1, V1])(fn: (K1, V1) => Future[(K2, V2)]): Future[Map[K2, V2]] =
      Future.traverse[(K1, V1), (K2, V2), Iterable](in)({
        case (k1: K1, v1: V1) => fn(k1, v1)
      }).map(_.toMap)

    def load(): Future[TileImageBank] =
      traverseMap(tileIndexToPidFilename) {
        case (tileIndex: Int, pidFilename: String) => {
          val pngFilename = pidFilename.replaceFirst(".PID", ".png")
          val pngSrc = s"/assets/images/CLAW/LEVEL1/TILES/ACTION/${pngFilename}"
          loadImage(pngSrc).map(pngImage => (tileIndex, pngImage))
        }
      }.map(tileIndexToPngImage =>
        new TileImageBank(tileIndexToPngImage)
      )
  }


  def canvasView(drawFn: Cell[CanvasRenderingContext2D => Unit]): Widget = {
    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.style = "width: 100%; height: 100%;"

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    var isDirty = false

    drawFn.listen(_ => {
      isDirty = true
    })

    def requestDraw(): Unit = {
      window.requestAnimationFrame(t => {
        val rect = canvas.getBoundingClientRect()

        val w = rect.width.toInt
        val h = rect.height.toInt

        if (canvas.width != w || canvas.height != h) {
          canvas.width = w
          canvas.height = h
        }

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

  def tilesViewOuter(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic
    import firesword.frp.Frp.implicitConstSome

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(tilesView(editor))
    )

    def calculateTargetPoint(e: PointerEvent): Vec2 = {
      val rect = tilesViewDiv.node.getBoundingClientRect()
      val x = e.clientX - rect.left
      val y = e.clientY - rect.top

      Vec2(x, y)
    }

    tilesViewDiv.onPointerDown.listen(e => {
      val cameraState = editor.camera.state.sample()

      cameraState match {
        case freeCamera: FreeCamera =>
          val targetPoint = tilesViewDiv.onPointerMove.hold(e)
            .map(calculateTargetPoint)

          freeCamera.dragCamera(
            targetPoint = targetPoint,
            stop = tilesViewDiv.onPointerUp.map(_ => ()),
          )
      }
    })

    tilesViewDiv
  }


  def tilesView(editor: Editor): Widget = {
    val tiles = editor.tiles.content.sample()
    val objects = editor.objects

    val drawFn = Cell.map2(
      editor.cameraFocusPoint,
      editor.cameraZoom,
      (fp: Vec2, z: Double) =>
        (ctx: CanvasRenderingContext2D) => {
          import Transform._

          val canvas = ctx.canvas
          val canvasSize = Vec2(canvas.width, canvas.height)

          val camera = translate(canvasSize / 2) * scale(z) * translate(fp * -1)

          val transform = camera

          ctx.setTransform(
            transform.a,
            transform.b,
            transform.c,
            transform.d,
            transform.e,
            transform.f,
          )

          tiles foreach {
            case (coord, tile) => {
              val tileImage = editor.tileImageBank.getTileImage(tile)
              ctx.drawImage(tileImage, coord.j * 64, coord.i * 64)
            }
          }


          objects foreach (obj => {
            val pos = obj.position
            val fqImageSetId = obj.imageSetId.replaceFirst("LEVEL_", "LEVEL1_IMAGES_")

            val imageSetOpt = editor.rezIndex.getImageSet(fqImageSetId)
            val i = obj.wwdObject.i
            val textureOpt = imageSetOpt.flatMap(imageSet => imageSet.getTexture(i))

            textureOpt.foreach(texture => {

              val image = texture.htmlImage
              val size = Vec2(image.width, image.height)
              val halfSize = size / 2


              val center = translate(halfSize * -1)

              val sx: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Mirror) != 0) -1 else 1
              val sy: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Invert) != 0) -1 else 1
              val mirror = scale(Vec2(sx, sy))

              val position = translate(obj.position + texture.offset)

              val transform = camera * position * mirror * center

              ctx.setTransform(
                transform.a,
                transform.b,
                transform.c,
                transform.d,
                transform.e,
                transform.f,
              )

              ctx.drawImage(image, 0, 0)
            })
          })
        }
    )

    canvasView(drawFn)
  }
}
