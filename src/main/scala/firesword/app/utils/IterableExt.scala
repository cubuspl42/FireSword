package firesword.app.utils

import scala.language.implicitConversions

object IterableExt {
  class IterableExt[A](self: Iterable[A]) {
    def mapSome[B](f: A => Option[B]): Iterable[B] =
      self.flatMap(a => f(a).fold(List[B]())(b => List(b)))
  }

  implicit def implicitIterableExt[A](self: Iterable[A]): IterableExt[A] =
    new IterableExt(self)

}
