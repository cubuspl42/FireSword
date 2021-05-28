package firesword.app.editor

import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.{World, WwdHeaderFlags, WwdPlaneFlags}

class WorldProperties(
                       wwdWorld: World,
                     ) {
  private def flagMutCell(flag: Int) =
    new MutCell((wwdWorld.flags & flag) != 0)

  val compress: MutCell[Boolean] = flagMutCell(WwdHeaderFlags.COMPRESS)

  val useZCoords: MutCell[Boolean] = flagMutCell(WwdHeaderFlags.USE_Z_COORDS)

  val name = new MutCell(wwdWorld.name.decode())

  val author = new MutCell(wwdWorld.author.decode())

  val dateCreatedString = new MutCell(wwdWorld.dateCreatedString.decode())

  val rezFilePath = new MutCell(wwdWorld.rezFilePath.decode())

  val imageDir = new MutCell(wwdWorld.imageDir.decode())

  val palRez = new MutCell(wwdWorld.palRez.decode())

  val startX = new MutCell(wwdWorld.startX)

  val startY = new MutCell(wwdWorld.startY)

  val launchApp = new MutCell(wwdWorld.launchApp.decode())

  val imageSet1 = new MutCell(wwdWorld.imageSet1.decode())

  val imageSet2 = new MutCell(wwdWorld.imageSet2.decode())

  val imageSet3 = new MutCell(wwdWorld.imageSet3.decode())

  val imageSet4 = new MutCell(wwdWorld.imageSet4.decode())

  val prefix1 = new MutCell(wwdWorld.prefix1.decode())

  val prefix2 = new MutCell(wwdWorld.prefix2.decode())

  val prefix3 = new MutCell(wwdWorld.prefix3.decode())

  val prefix4 = new MutCell(wwdWorld.prefix4.decode())
}
