package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.CanvasView.canvasView
import firesword.app.EdObject.EdObject
import firesword.app.Editor.Editor
import firesword.app.Geometry.Vec2d
import firesword.app.utils.CanvasRenderingContext2DUtils.strokeRoundedRect
import firesword.dom.Dom.Tag.div
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import firesword.wwd.Wwd.DrawFlags
import org.scalajs.dom._

import scala.language.implicitConversions

object WorldView {
  def widgetV(widget: Widget, e: MouseEvent): Vec2d = {
    val rect = widget.node.getBoundingClientRect()
    val x = e.clientX - rect.left
    val y = e.clientY - rect.top
    Vec2d(x, y)
  }

  def worldViewOuter(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic
    import firesword.frp.Frp.implicitConstSome

    val tilesViewDiv = div(
      styleClass = MyStyles.tilesView,
      children = List(worldView(editor))
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
  
  def worldView(editor: Editor): Widget = {
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
                  ): Unit = {
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

    val drawFn = Cell.map2(
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

    val theView = canvasView(drawFn)

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
}