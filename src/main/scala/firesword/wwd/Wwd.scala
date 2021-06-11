package firesword.wwd

import firesword.wwd.DataStream.{ByteString, DataStream}
import firesword.wwd.DumpWwd.WAP_TILE_TYPE_SINGLE
import firesword.wwd.Geometry.Rectangle
import firesword.wwd.Wwd.readTileDescriptions
import pako.Pako

import scala.scalajs.js.typedarray.{ArrayBuffer, Int32Array, Uint8Array}

object Wwd {
  case class World(
                    flags: Int,
                    name: ByteString,
                    author: ByteString,
                    dateCreatedString: ByteString,
                    rezFilePath: ByteString,
                    imageDir: ByteString,
                    palRez: ByteString,
                    startX: Int,
                    startY: Int,
                    launchApp: ByteString,
                    imageSet1: ByteString,
                    imageSet2: ByteString,
                    imageSet3: ByteString,
                    imageSet4: ByteString,
                    prefix1: ByteString,
                    prefix2: ByteString,
                    prefix3: ByteString,
                    prefix4: ByteString,
                    planes: List[Plane],
                    tileDescriptions: List[TileDescription],
                  )

  case class Plane(
                    flags: Int,
                    name: ByteString,
                    tileWidth: Int, /* tile's width in pixels */
                    tileHeight: Int, /* tile's height in pixels */
                    movementXPercent: Int,
                    movementYPercent: Int,
                    fillColor: Int,
                    zCoord: Int,
                    tilesWide: Int,
                    tilesHigh: Int,
                    tiles: Int32Array,
                    imageSets: List[ByteString],
                    objects: List[Object_],
                  )

  object DrawFlags extends Enumeration {
    val NoDraw: Int = 1 << 0
    val Mirror: Int = 1 << 1
    val Invert: Int = 1 << 2
    val Flash: Int = 1 << 3
  }

  case class Object_(
                      id: Int,
                      name: ByteString,
                      logic: ByteString,
                      imageSet: ByteString,
                      animation: ByteString,
                      x: Int,
                      y: Int,
                      z: Int,
                      i: Int,
                      addFlags: Int,
                      dynamicFlags: Int,
                      drawFlags: Int,
                      userFlags: Int,
                      score: Int,
                      points: Int,
                      powerUp: Int,
                      damage: Int,
                      smarts: Int,
                      health: Int,
                      moveRect: Rectangle,
                      hitRect: Rectangle,
                      attackRect: Rectangle,
                      clipRect: Rectangle,
                      userRect1: Rectangle,
                      userRect2: Rectangle,
                      userValue1: Int,
                      userValue2: Int,
                      userValue3: Int,
                      userValue4: Int,
                      userValue5: Int,
                      userValue6: Int,
                      userValue7: Int,
                      userValue8: Int,
                      xMin: Int,
                      yMin: Int,
                      xMax: Int,
                      yMax: Int,
                      speedX: Int,
                      speedY: Int,
                      xTweak: Int,
                      yTweak: Int,
                      counter: Int,
                      speed: Int,
                      width: Int,
                      height: Int,
                      direction: Int,
                      faceDir: Int,
                      timeDelay: Int,
                      frameDelay: Int,
                      objectType: Int,
                      hitTypeFlags: Int,
                      xMoveRes: Int,
                      yMoveRes: Int,
                    )

  object Object_ {
    def empty = Object_(
      id = 0,
      name = ByteString.empty(),
      logic = ByteString.empty(),
      imageSet = ByteString.empty(),
      animation = ByteString.empty(),
      x = 0,
      y = 0,
      z = 0,
      i = -1,
      addFlags = 0,
      dynamicFlags = 0,
      drawFlags = 0,
      userFlags = 0,
      score = 0,
      points = 0,
      powerUp = 0,
      damage = 0,
      smarts = 0,
      health = 0,
      moveRect = Rectangle.zero,
      hitRect = Rectangle.zero,
      attackRect = Rectangle.zero,
      clipRect = Rectangle.zero,
      userRect1 = Rectangle.zero,
      userRect2 = Rectangle.zero,
      userValue1 = 0,
      userValue2 = 0,
      userValue3 = 0,
      userValue4 = 0,
      userValue5 = 0,
      userValue6 = 0,
      userValue7 = 0,
      userValue8 = 0,
      xMin = 0,
      yMin = 0,
      xMax = 0,
      yMax = 0,
      speedX = 0,
      speedY = 0,
      xTweak = 0,
      yTweak = 0,
      counter = 0,
      speed = 0,
      width = 0,
      height = 0,
      direction = 0,
      faceDir = 0,
      timeDelay = 0,
      frameDelay = 0,
      objectType = 0,
      hitTypeFlags = 0,
      xMoveRes = 0,
      yMoveRes = 0,
    )
  }

  def emptyObject(): Object_ = {
    Object_(
      id = 0,
      name = ByteString.empty(),
      logic = ByteString.empty(),
      imageSet = ByteString.empty(),
      animation = ByteString.empty(),
      x = 0,
      y = 0,
      z = 0,
      i = 0,
      addFlags = 0,
      dynamicFlags = 0,
      drawFlags = 0,
      userFlags = 0,
      score = 0,
      points = 0,
      powerUp = 0,
      damage = 0,
      smarts = 0,
      health = 0,
      moveRect = Rectangle.fromBounds(0, 0, 0, 0),
      hitRect = Rectangle.fromBounds(0, 0, 0, 0),
      attackRect = Rectangle.fromBounds(0, 0, 0, 0),
      clipRect = Rectangle.fromBounds(0, 0, 0, 0),
      userRect1 = Rectangle.fromBounds(0, 0, 0, 0),
      userRect2 = Rectangle.fromBounds(0, 0, 0, 0),
      userValue1 = 0,
      userValue2 = 0,
      userValue3 = 0,
      userValue4 = 0,
      userValue5 = 0,
      userValue6 = 0,
      userValue7 = 0,
      userValue8 = 0,
      xMin = 0,
      yMin = 0,
      xMax = 0,
      yMax = 0,
      speedX = 0,
      speedY = 0,
      xTweak = 0,
      yTweak = 0,
      counter = 0,
      speed = 0,
      width = 0,
      height = 0,
      direction = 0,
      faceDir = 0,
      timeDelay = 0,
      frameDelay = 0,
      objectType = 0,
      hitTypeFlags = 0,
      xMoveRes = 0,
      yMoveRes = 0,
    )
  }

  object WwdHeaderFlags {
    val USE_Z_COORDS: Int = 1 << 0
    val COMPRESS: Int = 1 << 1
  }

  case class WwdHeader(
                        size: Int,
                        flags: Int,
                        levelName: ByteString,
                        author: ByteString,
                        birth: ByteString,
                        rezFile: ByteString,
                        imageDir: ByteString,
                        palRez: ByteString,
                        startX: Int,
                        startY: Int,
                        planeCount: Int,
                        mainBlockOffset: Int,
                        tileDescriptionsOffset: Int,
                        decompressedMainBlockSize: Int,
                        checksum: Int,
                        launchApp: ByteString,
                        imageSet1: ByteString,
                        imageSet2: ByteString,
                        imageSet3: ByteString,
                        imageSet4: ByteString,
                        prefix1: ByteString,
                        prefix2: ByteString,
                        prefix3: ByteString,
                        prefix4: ByteString,
                      )

  object WwdPlaneFlags {
    val MAIN_PLANE: Int = 1 << 0
    val NO_DRAW: Int = 1 << 1
    val X_WRAPPING: Int = 1 << 2
    val Y_WRAPPING: Int = 1 << 3
    val AUTO_TILE_SIZE: Int = 1 << 4
  }

  case class WwdPlaneHeader(
                             size: Int,
                             flags: Int,
                             name: ByteString,
                             widthPx: Int,
                             heightPx: Int,
                             tileWidth: Int, /* tile's width in pixels */
                             tileHeight: Int, /* tile's height in pixels */
                             movementXPercent: Int,
                             movementYPercent: Int,
                             fillColor: Int,
                             imageSetCount: Int, // *
                             objectCount: Int, // *
                             tilesOffset: Int, // *
                             imageSetsOffset: Int, // *
                             objectsOffset: Int, // *
                             zCoord: Int,
                             tilesWide: Int,
                             tilesHigh: Int,
                           )

  val levelNameLength = 64
  val authorLength = 64
  val birthLength = 64
  val rezFileLength = 256
  val imageDirLength = 128
  val palRezLength = 128
  val launchAppLength = 128
  val imageSetLength = 128
  val prefixLength = 32


  case class TileDescription(
                              _type: Int = 0, /* WAP_TILE_TYPE_ single value */
                              width: Int = 0, /* in pixels */
                              height: Int = 0, /* in pixels */
                              insideAttrib: Int = 0, /* WAP_TILE_ATTRIBUTE_ */
                              /* outside_attrib and rect only if type == WAP_TILE_TYPE_DOUBLE */
                              outsideAttr: Int = 0, /* WAP_TILE_ATTRIBUTE_ */
                              rect: Rectangle = Rectangle.zero,
                            )

  def range(end: Int): List[Int] =
    (0 until end).toList

  def concat(buffer1: ArrayBuffer, buffer2: ArrayBuffer): ArrayBuffer = {
    val tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength)
    tmp.set(new Uint8Array(buffer1), 0)
    tmp.set(new Uint8Array(buffer2), buffer1.byteLength)
    tmp.buffer
  }

  def inflate(buffer: Uint8Array): Uint8Array =
    Pako.inflate(new Uint8Array(buffer))

  def decompressBuffer(compressedBuffer: ArrayBuffer): ArrayBuffer = {
    val decompressedArray = inflate(new Uint8Array(compressedBuffer))
    decompressedArray.buffer
  }

  def readWorld(wwdBuffer: ArrayBuffer): World = {
    val header = readWwdHeader(wwdBuffer)
    val mainBlock = wwdBuffer.slice(header.mainBlockOffset)

    def decompressWwdBuffer(): ArrayBuffer = {
      val headerBuffer = wwdBuffer.slice(0, header.size)
      val decompressedMainBlock = decompressBuffer(mainBlock)
      concat(headerBuffer, decompressedMainBlock)
    }

    val decompressedWwdBuffer = if ((header.flags & WwdHeaderFlags.COMPRESS) != 0)
      decompressWwdBuffer() else wwdBuffer

    val planes = readPlanes(header, decompressedWwdBuffer)

    val tileDescriptions = readTileDescriptions(header, decompressedWwdBuffer)

    World(
      flags = header.flags,
      name = header.levelName,
      author = header.author,
      dateCreatedString = header.birth,
      rezFilePath = header.rezFile,
      imageDir = header.imageDir,
      palRez = header.palRez,
      startX = header.startX,
      startY = header.startY,
      launchApp = header.launchApp,
      imageSet1 = header.imageSet1,
      imageSet2 = header.imageSet2,
      imageSet3 = header.imageSet3,
      imageSet4 = header.imageSet4,
      prefix1 = header.prefix1,
      prefix2 = header.prefix2,
      prefix3 = header.prefix3,
      prefix4 = header.prefix4,
      planes = planes,
      tileDescriptions = tileDescriptions,
    )
  }

  def readWwdHeader(headerBuffer: ArrayBuffer): WwdHeader = {
    val stream = new DataStream(headerBuffer)

    val size = stream.readUint32() // header size
    stream.readUint32() // 0
    val flags = stream.readUint32()
    stream.readUint32() // 0
    val levelName = stream.readByteString(levelNameLength)
    val author = stream.readByteString(authorLength)
    val birth = stream.readByteString(birthLength)
    val rezFile = stream.readByteString(rezFileLength)
    val imageDir = stream.readByteString(imageDirLength)
    val palRez = stream.readByteString(palRezLength)
    val startX = stream.readInt32()
    val startY = stream.readInt32()
    stream.readUint32() // ?
    val planeCount = stream.readUint32()
    val mainBlockOffset = stream.readUint32()
    val tileDescriptionsOffset = stream.readUint32()
    val decompressedMainBlockSize = stream.readUint32()
    val checksum = stream.readUint32()
    stream.readUint32() // ?
    val launchApp = stream.readByteString(launchAppLength)
    val imageSet1 = stream.readByteString(imageSetLength)
    val imageSet2 = stream.readByteString(imageSetLength)
    val imageSet3 = stream.readByteString(imageSetLength)
    val imageSet4 = stream.readByteString(imageSetLength)
    val prefix1 = stream.readByteString(prefixLength)
    val prefix2 = stream.readByteString(prefixLength)
    val prefix3 = stream.readByteString(prefixLength)
    val prefix4 = stream.readByteString(prefixLength)


    println(s"world name: ${levelName.decode()}")

    WwdHeader(
      size = size.toInt, // Sign?
      flags = flags.toInt, // Sign?
      levelName = levelName,
      author = author,
      birth = birth,
      rezFile = rezFile,
      imageDir = imageDir,
      palRez = palRez,
      startX = startX,
      startY = startY,
      planeCount = planeCount.toInt,
      mainBlockOffset = mainBlockOffset.toInt,
      tileDescriptionsOffset = tileDescriptionsOffset.toInt,
      decompressedMainBlockSize = decompressedMainBlockSize.toInt,
      checksum = checksum.toInt,
      launchApp = launchApp,
      imageSet1 = imageSet1,
      imageSet2 = imageSet2,
      imageSet3 = imageSet3,
      imageSet4 = imageSet4,
      prefix1 = prefix1,
      prefix2 = prefix2,
      prefix3 = prefix3,
      prefix4 = prefix4
    )
  }

  def readPlanes(wwdHeader: WwdHeader, wwdBuffer: ArrayBuffer): List[Plane] = {
    val headers = readPlaneHeaders(wwdHeader, wwdBuffer)
    headers.map(header => readPlane(header, wwdBuffer))
  }

  def readPlaneHeaders(wwdHeader: WwdHeader, wwdBuffer: ArrayBuffer): List[WwdPlaneHeader] = {
    val stream = new DataStream(wwdBuffer, wwdHeader.mainBlockOffset)
    range(wwdHeader.planeCount).map(_ => readPlaneHeader(stream))
  }

  val planeNameBufferSize = 64

  def readPlaneHeader(stream: DataStream): WwdPlaneHeader = {
    val size = stream.readUint32()
    stream.readUint32() // 0
    val flags = stream.readUint32()
    stream.readUint32() // 0
    val name = stream.readByteString(planeNameBufferSize)
    val widthPx = stream.readUint32()
    val heightPx = stream.readUint32()
    val tileWidth = stream.readUint32()
    val tileHeight = stream.readUint32()
    val tilesWide = stream.readUint32()
    val tilesHigh = stream.readUint32()
    stream.readUint32() // 0
    stream.readUint32() // 0
    val movementXPercent = stream.readUint32()
    val movementYPercent = stream.readUint32()
    val fillColor = stream.readUint32()
    val imageSetCount = stream.readUint32()
    val objectCount = stream.readUint32()
    val tilesOffset = stream.readUint32()
    val imageSetsOffset = stream.readUint32()
    val objectsOffset = stream.readUint32()
    val zCoord = stream.readInt32()
    stream.readUint32() // 0
    stream.readUint32() // 0
    stream.readUint32() // 0

    println(s"imageSetCount: ${imageSetCount}")
    println(s"objectCount: ${objectCount}")

    WwdPlaneHeader(
      size = size.toInt,
      flags = flags.toInt,
      name = name,
      widthPx = widthPx.toInt,
      heightPx = heightPx.toInt,
      tileWidth = tileWidth.toInt,
      tileHeight = tileHeight.toInt,
      movementXPercent = movementXPercent.toInt,
      movementYPercent = movementYPercent.toInt,
      fillColor = fillColor.toInt,
      imageSetCount = imageSetCount.toInt,
      objectCount = objectCount.toInt,
      tilesOffset = tilesOffset.toInt,
      imageSetsOffset = imageSetsOffset.toInt,
      objectsOffset = objectsOffset.toInt,
      zCoord = zCoord,
      tilesWide = tilesWide.toInt,
      tilesHigh = tilesHigh.toInt
    )
  }

  def readPlane(header: WwdPlaneHeader, wwdBuffer: ArrayBuffer): Plane = {
    val tiles = readPlaneTiles(header, wwdBuffer)
    val imageSets = readPlaneImageSets(header, wwdBuffer)
    val objects = readPlaneObjects(header, wwdBuffer)
    val plane = Plane(
      header.flags,
      header.name,
      header.tileWidth,
      header.tileHeight,
      header.movementXPercent,
      header.movementYPercent,
      header.fillColor,
      header.zCoord,
      header.tilesWide,
      header.tilesHigh,
      tiles,
      imageSets,
      objects
    )
    plane
  }

  def readPlaneTiles(header: WwdPlaneHeader, wwdBuffer: ArrayBuffer): Int32Array = {
    val stream = new DataStream(wwdBuffer, header.tilesOffset)

    val h = header.tilesHigh
    val w = header.tilesWide
    val size = h * w
    val array = new Int32Array(size)

    for (i <- 0 until size) {
      array.set(i, stream.readInt32())
    }

    array
  }

  def readPlaneImageSets(header: WwdPlaneHeader, wwdBuffer: ArrayBuffer): List[ByteString] = {
    val stream = new DataStream(wwdBuffer, header.imageSetsOffset)
    range(header.imageSetCount).map(_ => stream.readByteStringNullTerminated())
  }

  def readPlaneObjects(header: WwdPlaneHeader, wwdBuffer: ArrayBuffer): List[Object_] = {
    val stream = new DataStream(wwdBuffer, header.objectsOffset)
    range(header.objectCount).map(_ => readObject(stream))
  }

  def readObject(stream: DataStream): Object_ = {
    val id = stream.readInt32()
    val nameLen = stream.readInt32()
    val logicLen = stream.readInt32()
    val imageSetLen = stream.readInt32()
    val animationLen = stream.readInt32()
    val x = stream.readInt32()
    val y = stream.readInt32()
    val z = stream.readInt32()
    val i = stream.readInt32()
    val addFlags = stream.readInt32()
    val dynamicFlags = stream.readInt32()
    val drawFlags = stream.readInt32()
    val userFlags = stream.readInt32()
    val score = stream.readInt32()
    val points = stream.readInt32()
    val powerUp = stream.readInt32()
    val damage = stream.readInt32()
    val smarts = stream.readInt32()
    val health = stream.readInt32()
    val moveRect = stream.readRectangle()
    val hitRect = stream.readRectangle()
    val attackRect = stream.readRectangle()
    val clipRect = stream.readRectangle()
    val userRect1 = stream.readRectangle()
    val userRect2 = stream.readRectangle()
    val userValue1 = stream.readInt32()
    val userValue2 = stream.readInt32()
    val userValue3 = stream.readInt32()
    val userValue4 = stream.readInt32()
    val userValue5 = stream.readInt32()
    val userValue6 = stream.readInt32()
    val userValue7 = stream.readInt32()
    val userValue8 = stream.readInt32()
    val xMin = stream.readInt32()
    val yMin = stream.readInt32()
    val xMax = stream.readInt32()
    val yMax = stream.readInt32()
    val speedX = stream.readInt32()
    val speedY = stream.readInt32()
    val xTweak = stream.readInt32()
    val yTweak = stream.readInt32()
    val counter = stream.readInt32()
    val speed = stream.readInt32()
    val width = stream.readInt32()
    val height = stream.readInt32()
    val direction = stream.readInt32()
    val faceDir = stream.readInt32()
    val timeDelay = stream.readInt32()
    val frameDelay = stream.readInt32()
    val objectType = stream.readInt32()
    val hitTypeFlags = stream.readInt32()
    val xMoveRes = stream.readInt32()
    val yMoveRes = stream.readInt32()

    val name = stream.readByteString(nameLen)
    val logic = stream.readByteString(logicLen)
    val imageSet = stream.readByteString(imageSetLen)
    val animation = stream.readByteString(animationLen)

    Object_(
      id = id,
      name = name,
      logic = logic,
      imageSet = imageSet,
      animation = animation,
      x = x,
      y = y,
      z = z,
      i = i,
      addFlags = addFlags,
      dynamicFlags = dynamicFlags,
      drawFlags = drawFlags,
      userFlags = userFlags,
      score = score,
      points = points,
      powerUp = powerUp,
      damage = damage,
      smarts = smarts,
      health = health,
      moveRect = moveRect,
      hitRect = hitRect,
      attackRect = attackRect,
      clipRect = clipRect,
      userRect1 = userRect1,
      userRect2 = userRect2,
      userValue1 = userValue1,
      userValue2 = userValue2,
      userValue3 = userValue3,
      userValue4 = userValue4,
      userValue5 = userValue5,
      userValue6 = userValue6,
      userValue7 = userValue7,
      userValue8 = userValue8,
      xMin = xMin,
      yMin = yMin,
      xMax = xMax,
      yMax = yMax,
      speedX = speedX,
      speedY = speedY,
      xTweak = xTweak,
      yTweak = yTweak,
      counter = counter,
      speed = speed,
      width = width,
      height = height,
      direction = direction,
      faceDir = faceDir,
      timeDelay = timeDelay,
      frameDelay = frameDelay,
      objectType = objectType,
      hitTypeFlags = hitTypeFlags,
      xMoveRes = xMoveRes,
      yMoveRes = yMoveRes,
    )
  }

  def readTileDescriptions(wwdHeader: WwdHeader, wwdBuffer: ArrayBuffer): List[TileDescription] = {
    val stream = new DataStream(wwdBuffer, wwdHeader.tileDescriptionsOffset)

    stream.expectInt32(32)
    stream.expectInt32(0)
    val numTileDescriptions = stream.readInt32()
    stream.expectInt32(0)
    stream.expectInt32(0)
    stream.expectInt32(0)
    stream.expectInt32(0)
    stream.expectInt32(0)

    range(numTileDescriptions).map(_ => loadTileDescription(stream))
  }

  def loadTileDescription(stream: DataStream): TileDescription = {
    val _type = stream.readInt32()
    val unknown = stream.readInt32()
    val width = stream.readInt32()
    val height = stream.readInt32()

    if (_type == WAP_TILE_TYPE_SINGLE) {
      val insideAttrib = stream.readInt32()

      TileDescription(
        _type = _type,
        width = width,
        height = height,
        insideAttrib = insideAttrib,
      )
    } else {
      val outsideAttr = stream.readInt32()
      val insideAttrib = stream.readInt32()
      val rect = stream.readRectangle()

      TileDescription(
        _type = _type,
        width = width,
        height = height,
        insideAttrib = insideAttrib,
        outsideAttr = outsideAttr,
        rect = rect,
      )
    }
  }
}
