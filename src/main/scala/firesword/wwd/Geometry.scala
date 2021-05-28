package firesword.wwd

object Geometry {
  case class Vec2(x: Int, y: Int) {
    def *(s: Int): Vec2 =
      Vec2(x * s, y * s)

    def /(s: Int): Vec2 =
      Vec2(x / s, y / s)

    def +(that: Vec2): Vec2 =
      Vec2(x + that.x, y + that.y)

    def -(that: Vec2): Vec2 =
      Vec2(x - that.x, y - that.y)

    def abs(): Vec2 = Vec2(x.abs, y.abs)

    def neg(): Vec2 = Vec2(-x, -y)

    def width: Int = x.abs

    def height: Int = y.abs
  }

  case class Rectangle(position: Vec2, size: Vec2) {
    def xMin: Int = position.x

    def yMin: Int = position.y

    def width: Int = size.width

    def height: Int = size.height


    def xMax: Int = {
      this.xMin + this.width
    }

    def yMax: Int = {
      this.yMin + this.height
    }

    def xyMin: Vec2 = {
      Vec2(this.xMin, this.yMin)
    }

    def xyMax: Vec2 = {
      Vec2(this.xMax, this.yMax)
    }

    def overlaps(b: Rectangle): Boolean = {
      this.xMin < b.xMax && b.xMin < this.xMax &&
        this.yMin < b.yMax && b.yMin < this.yMax
    }

    def map(f: Vec2 => Vec2): Rectangle = {
      Rectangle.fromDiagonal(
        f(this.xyMin),
        f(this.xyMax),
      )
    }
  }

  object Rectangle {
    def zero: Rectangle = Rectangle(Vec2(0, 0), Vec2(0, 0))

    def fromCenter(center: Vec2, size: Vec2): Rectangle = {
      val size_ = size.abs()
      new Rectangle(size_.neg() / 2, size_)
    }

    def fromBounds(left: Int, top: Int, right: Int, bottom: Int): Rectangle = {
      new Rectangle(Vec2(left, top), Vec2(right - left, bottom - top))
    }

    def fromDiagonal(a: Vec2, b: Vec2): Rectangle = {
      new Rectangle(Vec2(Math.min(a.x, b.x), Math.min(a.y, b.y)), b - a)
    }
  }
}
