package firesword.app

object Geometry {
  case class Vec2d(x: Double, y: Double) {
    def length: Double = Math.sqrt(x * x + y * y)

    def *(s: Double): Vec2d =
      Vec2d(x * s, y * s)

    def /(s: Double): Vec2d =
      Vec2d(x / s, y / s)

    def +(that: Vec2d): Vec2d =
      Vec2d(x + that.x, y + that.y)

    def -(that: Vec2d): Vec2d =
      Vec2d(x - that.x, y - that.y)
  }
}
