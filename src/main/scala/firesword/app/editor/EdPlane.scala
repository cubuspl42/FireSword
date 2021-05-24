package firesword.app.editor

import firesword.app.Geometry.Vec2d
import firesword.app.editor.EdObject.EdObject
import firesword.app.utils.IntMatrixMap
import firesword.frp.DynamicSet
import firesword.frp.DynamicSet.DynamicSet
import firesword.wwd.Wwd.Plane

object EdPlane {
  class EdPlane(
                 val wwdPlane: Plane,
               ) {
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

    def name = wwdPlane.name.decode()

    def imageSets: List[String] = wwdPlane.imageSets.map(_.decode())

    // TODO: Figure out the algorithm
    def primaryImageSet = imageSets.head

    println(s"Plane ${name} imageSets: ${wwdPlane.imageSets}")
  }
}
