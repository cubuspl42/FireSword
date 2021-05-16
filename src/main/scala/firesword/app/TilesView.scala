package firesword.app

import firesword.app
import firesword.app.Camera.FreeCamera
import firesword.app.EdObject.EdObject
import firesword.app.Editor.{Editor, Vec2}
import firesword.app.Transform.{scale, translate}
import firesword.dom.Dom.Tag.div
import firesword.dom.Dom.Widget
import firesword.frp.{Cell, DynamicList}
import firesword.frp.Cell.Cell
import firesword.frp.Frp.Const
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

  def widgetV(widget: Widget, e: MouseEvent): Vec2 = {
    //    val rect = widget.node.getBoundingClientRect()
    //    val x = e.clientX - rect.left
    //    val y = e.clientY - rect.top

    val x = e.clientX
    val y = e.clientY

    Vec2(x, y)
  }

  def tilesViewOuter(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic
    import firesword.frp.Frp.implicitConstSome

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(tilesView(editor))
    )

    def calculateTargetPoint(e: MouseEvent): Vec2 = {
      widgetV(tilesViewDiv, e)
    }

    tilesViewDiv.onPointerDown.listen(e => {
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
      (fp: Vec2, z: Double) => {
        //  translate(canvasSize / 2) *
        scale(z) * translate(fp * -1)
      },
    );

    def drawObject(ctx: CanvasRenderingContext2D, cameraTransform: Transform, obj: EdObject) = {
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

        val position = translate(obj.position.sample() + texture.offset)

        val camera = cameraTransform
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
    }

    val objectsDrawFns = objects
      .sortedBy(obj => obj.z)
      //        .toList()
      .fuseMap(obj => {
        //        console.log("@@@ 2")

        obj.position.map(pos => (ctx: CanvasRenderingContext2D, cameraTransform: Transform) => {
          drawObject(ctx, cameraTransform, obj)
        })
        //          Const((ctx: CanvasRenderingContext2D) => {})
      })

    //    val drawFn = cameraTransform.switchMapC(cameraTransform => {
    //
    //      println("@@@ 1")
    //
    //      def drawTiles(ctx: CanvasRenderingContext2D): Unit = {
    //        val camera = cameraTransform
    //
    //        val transform = camera
    //
    //        ctx.setTransform(
    //          transform.a,
    //          transform.b,
    //          transform.c,
    //          transform.d,
    //          transform.e,
    //          transform.f,
    //        )
    //
    //        tiles foreach {
    //          case (coord, tile) => {
    //            val tileImage = editor.tileImageBank.getTileImage(tile)
    //            ctx.drawImage(tileImage, coord.j * 64, coord.i * 64)
    //          }
    //        }
    //      }
    //
    //
    //      val objectsDrawFns = objects
    //        .sortedBy(obj => obj.z)
    //        //        .toList()
    //        .fuseMap(obj => {
    //          //          console.log("@@@ 2")
    //
    //
    //          obj.position.map(pos => (ctx: CanvasRenderingContext2D) => {
    //            val fqImageSetId = obj.imageSetId.replaceFirst("LEVEL_", "LEVEL1_IMAGES_")
    //
    //            val imageSetOpt = editor.rezIndex.getImageSet(fqImageSetId)
    //            val i = obj.wwdObject.i
    //            val textureOpt = imageSetOpt.flatMap(imageSet => imageSet.getTexture(i))
    //
    //            textureOpt.foreach(texture => {
    //
    //              val image = texture.htmlImage
    //              val size = Vec2(image.width, image.height)
    //              val halfSize = size / 2
    //
    //
    //              val center = translate(halfSize * -1)
    //
    //              val sx: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Mirror) != 0) -1 else 1
    //              val sy: Int = if ((obj.wwdObject.drawFlags & DrawFlags.Invert) != 0) -1 else 1
    //              val mirror = scale(Vec2(sx, sy))
    //
    //              val position = translate(obj.position.sample() + texture.offset)
    //
    //              val camera = cameraTransform
    //              val transform = camera * position * mirror * center
    //
    //              ctx.setTransform(
    //                transform.a,
    //                transform.b,
    //                transform.c,
    //                transform.d,
    //                transform.e,
    //                transform.f,
    //              )
    //
    //              ctx.drawImage(image, 0, 0)
    //            })
    //          })
    //
    //          //          Const((ctx: CanvasRenderingContext2D) => {})
    //        })
    //
    //      //      val objectsDrawFns = DynamicList.empty[CanvasRenderingContext2D => Unit]()
    //
    //      objectsDrawFns.content.map(drawFns => (ctx: CanvasRenderingContext2D) => {
    //        //        drawTiles(ctx)
    //        drawFns.take(500).foreach(drawFn => drawFn(ctx))
    //      })
    //    })

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
          val p = Vec2(coord.j * 64, coord.i * 64)
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

        theView.onPointerDown.listen(e => {
          val viewPoint = widgetV(theView, e)
          val invertedTransform = cameraTransform.sample().inversed()
          val worldPoint = invertedTransform.transform(viewPoint)
          val obj = editor.findClosestObject(worldPoint)

          obj.move(Vec2(32, 0))

          println(obj.wwdObject.id)
        })

    theView
  }
}
