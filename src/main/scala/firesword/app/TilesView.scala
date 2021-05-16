package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.EdObject.EdObject
import firesword.app.Editor.Editor
import firesword.app.Geometry.Vec2d
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

  def widgetV(widget: Widget, e: MouseEvent): Vec2d = {
    //    val rect = widget.node.getBoundingClientRect()
    //    val x = e.clientX - rect.left
    //    val y = e.clientY - rect.top

    val x = e.clientX
    val y = e.clientY

    Vec2d(x, y)
  }

  def tilesViewOuter(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic
    import firesword.frp.Frp.implicitConstSome

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(tilesView(editor))
    )

    def calculateTargetPoint(e: MouseEvent): Vec2d = {
      widgetV(tilesViewDiv, e)
    }

    tilesViewDiv.onMouseDown.listen(e => {
      if (e.button == 2) { // Secondary mouse button
        val cameraState = editor.camera.state.sample()

        cameraState match {
          case freeCamera: FreeCamera =>
            val targetPoint = tilesViewDiv.onMouseMove.hold(e)
              .map(calculateTargetPoint)

            freeCamera.dragCamera(
              targetPoint = targetPoint,
              stop = tilesViewDiv.onPointerUp.map(_ => ()),
            )
        }
      }
    })

    tilesViewDiv
  }


  def tilesView(editor: Editor): Widget = {
    import Transform._


    val tiles = editor.tiles.content.sample()
    //      .take(1000)

    val objects = editor.objects


    val cameraTransform = Cell.map2(
      editor.cameraFocusPoint,
      editor.cameraZoom,
      (fp: Vec2d, z: Double) => {
        //  translate(canvasSize / 2) *
        scale(z) * translate(fp * -1)
      },
    );

    def drawObject(
                    ctx: CanvasRenderingContext2D,
                    cameraTransform: Transform,
                    obj: EdObject,
                    position: Vec2d,
                    isSelected: Boolean,
                  ) = {
      val fqImageSetId = obj.imageSetId.replaceFirst("LEVEL_", "LEVEL1_IMAGES_")

      val imageSetOpt = editor.rezIndex.getImageSet(fqImageSetId)
      val i = obj.wwdObject.i
      val textureOpt = imageSetOpt.flatMap(imageSet => imageSet.getTexture(i))

      textureOpt.foreach(texture => {

        val image = texture.htmlImage
        val size = Vec2d(image.width, image.height)
        val halfSize = size / 2


        val center = translate(halfSize * -1)

        val sx: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Mirror) != 0) -1 else 1
        val sy: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Invert) != 0) -1 else 1
        val mirror = scale(Vec2d(sx, sy))

        val positionTransform = translate(position + texture.offset)

        val transform = cameraTransform * positionTransform * mirror * center

        ctx.setTransform(
          transform.a,
          transform.b,
          transform.c,
          transform.d,
          transform.e,
          transform.f,
        )

        ctx.drawImage(image, 0, 0)

        if (isSelected) {
          ctx.strokeStyle = "red"
          ctx.lineWidth = 2.0
          strokeRoundedRect(ctx, 0, 0, image.width, image.height, 4)
        }
      })
    }

    val objectsDrawFns = objects
      .sortedBy(obj => obj.z)
      .fuseMap(obj => {


        Cell.map2(
          obj.position,
          editor.selectedObject.map(_.contains(obj)),
          (
            position: Vec2d,
            isSelected: Boolean,
          ) => (ctx: CanvasRenderingContext2D, cameraTransform: Transform) => {
            drawObject(
              ctx,
              cameraTransform,
              obj,
              position,
              isSelected,
            )
          },
        )
      })

    def drawTiles(ctx: CanvasRenderingContext2D, cameraTransform: Transform): Unit = {
      val canvas = ctx.canvas
      val camera = cameraTransform

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
          val p = Vec2d(coord.j * 64, coord.i * 64)
          val cp = cameraTransform.transform(p)

          if (cp.x >= -64 && cp.x < canvas.width && cp.y >= -64 && cp.y < canvas.height) {
            ctx.drawImage(tileImage, coord.j * 64, coord.i * 64)
          }
        }
      }
    }

    val drawFn_ = Cell.map2(
      cameraTransform,
      objectsDrawFns.content,
      (
        cameraTransform: Transform,
        objectsDrawFns: List[(CanvasRenderingContext2D, Transform) => Unit],
      ) => (ctx: CanvasRenderingContext2D) => {
        drawTiles(ctx, cameraTransform)
        objectsDrawFns.foreach(drawFn => drawFn(ctx, cameraTransform))
      }
    )

    val theView = canvasView(drawFn_)

    theView.onMouseDown.listen(e => {
      if (e.button == 0) {
        val viewPoint = widgetV(theView, e)
        val invertedTransform = cameraTransform.sample().inversed()
        val worldPoint = invertedTransform.transform(viewPoint)
        val obj = editor.findClosestObject(worldPoint)

        editor.selectClosestObject(worldPoint)

        println(obj.wwdObject.id)
      }
    })

    theView
  }

  private def strokeRoundedRect(
                                 ctx: CanvasRenderingContext2D,
                                 x: Double,
                                 y: Double,
                                 width: Double,
                                 height: Double,
                                 radius: Double,
                               ) {
    ctx.beginPath();
    ctx.moveTo(x + radius, y);
    ctx.lineTo(x + width - radius, y);
    ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
    ctx.lineTo(x + width, y + height - radius);
    ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
    ctx.lineTo(x + radius, y + height);
    ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
    ctx.lineTo(x, y + radius);
    ctx.quadraticCurveTo(x, y, x + radius, y);
    ctx.closePath();

    ctx.stroke();
  }
}
