package firesword.app

import firesword.app.Editor.Vec2
import firesword.dom.Dom.Widget
import firesword.frp.Cell.Cell
import firesword.frp.DynamicSet.DynamicSet

object SceneView {
  def sceneView(
                 cameraFocusPoint: Cell[Vec2],
                 cameraZoom: Cell[Double],
                 sprites: DynamicSet[Sprite]
               ): Widget = {
    ???
  }

  class Sprite(
                val z: Cell[Double],
              ) {

  }
}
