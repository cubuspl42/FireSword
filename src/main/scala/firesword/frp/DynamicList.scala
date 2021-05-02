package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.Frp.Const

import scala.language.implicitConversions

object DynamicList {
  class DynamicList[+A](val content: Cell[List[A]]) {
    def map[B](f: A => B): DynamicList[B] =
      new DynamicList(content.map(_.map(f)))
  }

  implicit def implicitDynamicList[A](list: List[A]): DynamicList[A] = {
    new DynamicList(Const(list))
  }
}
