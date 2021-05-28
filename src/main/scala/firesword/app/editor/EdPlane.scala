package firesword.app.editor

import firesword.app.Geometry.Vec2d
import firesword.app.editor.EdObject.EdObject
import firesword.app.utils.IntMatrixMap
import firesword.frp.DynamicSet
import firesword.frp.DynamicSet.DynamicSet
import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.{Plane, WwdPlaneFlags}

object EdPlane {
  class EdPlane(
                 val wwdPlane: Plane,
               ) {
    private def flagMutCell(flag: Int) =
      new MutCell((wwdPlane.flags & flag) != 0)

    val mainPlane: MutCell[Boolean] = flagMutCell(WwdPlaneFlags.MAIN_PLANE)

    val noDraw: MutCell[Boolean] = flagMutCell(WwdPlaneFlags.NO_DRAW)

    val xWrapping: MutCell[Boolean] = flagMutCell(WwdPlaneFlags.X_WRAPPING)

    val yWrapping: MutCell[Boolean] = flagMutCell(WwdPlaneFlags.Y_WRAPPING)

    val autoTileSize: MutCell[Boolean] = flagMutCell(WwdPlaneFlags.AUTO_TILE_SIZE)

    val name = new MutCell(wwdPlane.name.decode())

    val tileWidth = new MutCell(wwdPlane.tileWidth)

    val tileHeight = new MutCell(wwdPlane.tileWidth)

    val movementXPercent = new MutCell(wwdPlane.movementXPercent)

    val movementYPercent = new MutCell(wwdPlane.movementYPercent)

    val fillColor = new MutCell(wwdPlane.fillColor)

    val tiles = new IntMatrixMap(
      width = wwdPlane.tilesWide,
      height = wwdPlane.tilesHigh,
      array = wwdPlane.tiles,
    )

    val objects: DynamicSet[EdObject] = DynamicSet.of(
      wwdPlane.objects
        .map(wwdObject => new EdObject(
          wwdObject = wwdObject,
          initialPosition = Vec2d(wwdObject.x, wwdObject.y),
        )).toSet
    )

    def findClosestObject(p: Vec2d): EdObject =
      objects.content.sample().minBy(obj => (obj.position.sample() - p).length)

    def imageSets: List[String] = wwdPlane.imageSets.map(_.decode())

    // TODO: Figure out the algorithm
    def primaryImageSet: String = imageSets.head

    private var _savedCameraFocusPoint = Vec2d(0, 0)

    private[editor] def saveCameraFocusPoint(fp: Vec2d): Unit = {
      _savedCameraFocusPoint = fp
    }

    def savedCameraFocusPoint: Vec2d = _savedCameraFocusPoint
  }
}
