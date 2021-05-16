package firesword.app

import firesword.app.Geometry.Vec2d

case class Transform(
                      a: Double,
                      b: Double,
                      c: Double,
                      d: Double,
                      e: Double,
                      f: Double,
                    ) {


  def *(that: Transform): Transform = {
    val m1 = this
    val m2 = that
    Transform(
      a = m1.a * m2.a + m1.c * m2.b,
      c = m1.a * m2.c + m1.c * m2.d,
      e = m1.a * m2.e + m1.c * m2.f + m1.e,
      b = m1.b * m2.a + m1.d * m2.b,
      d = m1.b * m2.c + m1.d * m2.d,
      f = m1.b * m2.e + m1.d * m2.f + m1.f,
    )
  }

  def inversed(): Transform = {
    val de = a * d - b * c
    Transform(
      a = d / de,
      b = b / -de,
      c = c / -de,
      d = a / de,
      e = (d * e - c * f) / -de,
      f = (b * e - a * f) / de
    )
  }

  def transform(v: Vec2d): Vec2d =
    Vec2d(
      x = a * v.x + c * v.y + e,
      y = b * v.x + d * v.y + f,
    )

  //  def scaled(s: Vec2d): Transform =
  //    Transform.scale(s) * this
  //
  //  def scaledAround(s: Vec2d, c: Vec2d): Transform =
  //    Transform.scale(s) * this
}

object Transform {
  def identity(): Transform = Transform(
    a = 1,
    c = 0,
    e = 0,
    b = 0,
    d = 1,
    f = 0,
  )


  def scale(s: Vec2d): Transform = Transform(
    a = s.x,
    c = 0,
    e = 0,
    b = 0,
    d = s.y,
    f = 0,
  )


  def scale(s: Double): Transform = scale(Vec2d(s, s))

  def scaleAround(s: Vec2d, c: Vec2d): Transform = {
    val st = scale(s)
    translate(c) * st * translate(c * -1)
  }

  def scaleAround(s: Double, c: Vec2d): Transform =
    scaleAround(Vec2d(s, s), c)

  def translate(t: Vec2d): Transform = Transform(
    a = 1,
    c = 0,
    e = t.x,
    b = 0,
    d = 1,
    f = t.y,
  )
}
