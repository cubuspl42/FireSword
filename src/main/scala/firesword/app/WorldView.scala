package firesword.app

import firesword.app.Camera.FreeCamera
import firesword.app.CanvasView.canvasView
import firesword.app.editor.EdObject.EdObject
import firesword.app.editor.EdPlane.EdPlane
import firesword.app.editor.Editor.{Editor, ObjectMode, TileCoord, TileMode}
import firesword.app.Geometry.Vec2d
import firesword.app.editor.Editor
import firesword.app.utils.CanvasRenderingContext2DUtils.strokeRoundedRect
import firesword.app.utils.IterableExt.implicitIterableExt
import firesword.dom.Dom.Tag.div
import firesword.dom.Dom.{MouseGesture, Widget}
import firesword.frp.{Cell, MutCell, Till}
import firesword.frp.Cell.Cell
import firesword.frp.Frp.Const
import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.DrawFlags
import org.scalajs.dom._

import scala.language.implicitConversions

object WorldView {
  def worldViewOuter(editor: Editor): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStatic
    import firesword.frp.Frp.implicitConstSome

    val worldViewDiv = div(
      styleClass = MyStyles.worldView,
      children = List(worldView(editor))
    )

    worldViewDiv.onMouseDown.listen(e => {
      if (e.button == 2) { // Secondary mouse button
        val cameraState = editor.camera.state.sample()

        cameraState match {
          case freeCamera: FreeCamera =>
            val targetPoint = worldViewDiv.onMouseMove.hold(e)
              .map(worldViewDiv.calculateRelativePosition)

            freeCamera.dragCamera(
              targetPoint = targetPoint,
              stop = worldViewDiv.onPointerUp.map(_ => ()),
            )
          case _ => ()
        }
      }
    })

    worldViewDiv
  }

  def worldView(editor: Editor): Widget = {
    import Transform._

    val canvasSize = new MutCell(Vec2d.zero)

    val cameraTransform = Cell.map3(
      editor.cameraFocusPoint,
      editor.cameraZoom,
      canvasSize,
      (fp: Vec2d, z: Double, canvasSize: Vec2d) => {
        translate(canvasSize / 2) * scale(z) * translate(fp * -1)
      },
    )

    val inversedCameraTransform = cameraTransform.map(_.inversed())

    def expandShortImageSetId(shortImageSetId: String): Option[String] = {
      def expandPrefix(prefix: String, expansion: String) = {
        val sanitizedExpansion = expansion.replace('\\', '_')
        if (shortImageSetId.startsWith(prefix))
          Some(shortImageSetId.replace(prefix, sanitizedExpansion))
        else None
      }

      editor.prefixMap.mapSome {
        case (prefix, expansion) => expandPrefix(prefix, expansion)
      }.headOption
    }

    def drawObject(
                    ctx: CanvasRenderingContext2D,
                    cameraTransform: Transform,
                    plane: EdPlane,
                    obj: EdObject,
                    position: Vec2d,
                    shortImageSetId: String,
                    isSelected: Boolean,
                    isEdited: Boolean,
                  ): Unit = {
      //      val fqImageSetId = obj.imageSetId.replaceFirst("LEVEL_", "LEVEL1_IMAGES_")

      //      val shortImageSetId = obj.imageSet.sample()
      //      val shortImageSetId = "LEVEL_OFFICER"

      val imageSetOpt = expandShortImageSetId(shortImageSetId).flatMap(
        fqImageSetId => editor.rezIndex.getImageSet(fqImageSetId)
      )

      def placeholderTexture() =
        editor.rezIndex.getImageSet("CLAW_IMAGES_RACER")
          .flatMap(imageSet => imageSet.getTexture(-1))

      val i = obj.wwdObject.i

      val textureOpt = imageSetOpt.flatMap(imageSet => imageSet.getTexture(i))
        .orElse(placeholderTexture())

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
          ctx.strokeStyle = if (isEdited) "blue" else "red"
          ctx.lineWidth = 2.0
          strokeRoundedRect(ctx, 0, 0, image.width, image.height, 4)
        }
      })
    }

    def buildDrawPlaneFn(
                          plane: EdPlane,
                        ) = {

      val tiles = plane.tiles

      val objects = plane.objects

      val objectsDrawFns = objects
        .sortedBy(obj => obj.z.map(_.toDouble))
        .fuseMap(obj => {
          Cell.map4(
            obj.position,
            obj.imageSet,
            editor.selectedObject.map(_.contains(obj)),
            editor.editedObject.map(_.contains(obj)),
            (
              position: Vec2d,
              imageSet: String,
              isSelected: Boolean,
              isEdited: Boolean,
            ) => (ctx: CanvasRenderingContext2D, cameraTransform: Transform) => {
              drawObject(
                ctx = ctx,
                cameraTransform = cameraTransform,
                plane = plane,
                obj = obj,
                position = position,
                shortImageSetId = imageSet,
                isSelected = isSelected,
                isEdited = isEdited,
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

        tiles.forEach {
          case (i, j, tile) =>
            if (tile > 0) {
              val tileImage = editor.tileImageBank.getTileImage(plane.primaryImageSet, tile)
              val p = Vec2d(j * 64, i * 64)
              val cp = cameraTransform.transform(p)

              if (cp.x >= -64 && cp.x < canvas.width && cp.y >= -64 && cp.y < canvas.height) {
                ctx.drawImage(tileImage, j * 64, i * 64)
              }
            }
        }


      }

      val tileModeHoverCoord = editor.mode.switchMapC {
        case ObjectMode => Const(None)
        case tm: TileMode => tm._hoverCoord
      }

      val drawFn = Cell.map4(
        cameraTransform,
        objectsDrawFns.content,
        plane.tiles.marker,
        tileModeHoverCoord,
        (
          cameraTransform: Transform,
          objectsDrawFns: List[(CanvasRenderingContext2D, Transform) => Unit],
          tileMarker: Unit,
          tileModeHoverCoordOpt: Option[TileCoord],
        ) => (ctx: CanvasRenderingContext2D) => {
          val canvas = ctx.canvas
          canvasSize.set(Vec2d(canvas.width, canvas.height))

          drawTiles(ctx, cameraTransform)
          objectsDrawFns.foreach(drawFn => drawFn(ctx, cameraTransform))


          tileModeHoverCoordOpt.foreach(tc => {
            val transform = cameraTransform

            ctx.setTransform(
              transform.a,
              transform.b,
              transform.c,
              transform.d,
              transform.e,
              transform.f,
            )

            ctx.strokeRect(tc.j * 64, tc.i * 64, 64, 64)
          })
        }
      )

      drawFn
    }


    val drawFn =
      editor.activePlane.switchMapC(buildDrawPlaneFn)

    val theView = canvasView(drawFn)

    def handleMouseDownObjectMode(e: MouseEvent): Unit = {
      if (e.button == 0) {
        val viewPoint = theView.calculateRelativePosition(e)
        val invertedTransform = inversedCameraTransform.sample()
        val initialWorldPoint = invertedTransform.transform(viewPoint)

        val activePlane = editor.activePlane.sample()
        val obj = activePlane.findClosestObject(initialWorldPoint)

        val isSelected = editor.selectedObject.sample().contains(obj)

        if (isSelected) {
          val gesture = MouseGesture.startDrag(
            element = theView,
            event = e,
            tillAbort = Till.end,
          )

          val targetWorldPoint = Cell.map2(
            inversedCameraTransform,
            gesture.clientPos.map(theView.calculateRelativePosition),
            (
              transform: Transform,
              clientPos: Vec2d,
            ) => transform.transform(clientPos)
          )

          val delta = targetWorldPoint.map(twp => {
            twp - initialWorldPoint
          })

          obj.move(delta = delta, commit = gesture.tillEnd.on)
        } else {
          editor.selectObject(obj)
        }
      }

      if (e.button == 1) {
        editor.selectedObject.sample().foreach(editor.editObject)
      }
    }

    def handleMouseDownTileMode(e: MouseEvent): Unit = {
      if (e.button == 0) {
        val gesture = MouseGesture.startDrag(theView, e, tillAbort = Till.end)

        val targetWorldPoint = Cell.map2(
          inversedCameraTransform,
          gesture.clientPos.map(theView.calculateRelativePosition),
          (
            transform: Transform,
            clientPos: Vec2d,
          ) => transform.transform(clientPos)
        )

        targetWorldPoint.listenTill(twp => {
          editor.drawTileAt(twp)
        }, till = gesture.tillEnd)
      }
    }

    theView.onMouseDown.listen(e => {
      editor.mode.sample() match {
        case ObjectMode =>
          handleMouseDownObjectMode(e)
        case _: TileMode =>
          handleMouseDownTileMode(e)
      }
    })

    theView.onMouseHover.listen(gesture => {
      gesture.clientPos.listenTill(cp => {
        val rp = theView.calculateRelativePosition(cp)
        val wp = inversedCameraTransform.sample().transform(rp)
        val tc = editor.getTileCoordAtPoint(wp)

        editor.mode.sample() match {
          case tileMode: TileMode => tileMode.hover(tc)
          case _ =>
        }
      }, till = gesture.tillEnd)
    })

    theView
  }
}
