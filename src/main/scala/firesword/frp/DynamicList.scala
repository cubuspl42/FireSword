package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.Frp.Const

import scala.language.implicitConversions

object DynamicList {
  class DynamicList[+A](val content: Cell[List[A]]) {
    def map[B](f: A => B): DynamicList[B] =
      new DynamicList(content.map(_.map(f)))
  }

  def empty[A](): DynamicList[A]=
    new DynamicList(Const(List()))

  def singleton[A](cell: Cell[A]): DynamicList[A]=
    new DynamicList(cell.map(List(_)))

  object Implicits {
    implicit def implicitSingleton[A](ca: Cell[A]): DynamicList[A] =
      singleton(ca)

    implicit def implicitStaticSingleton[A](a: A): DynamicList[A] =
      singleton(Const(a))

    implicit def implicitStatic[A](list: List[A]): DynamicList[A] = {
      new DynamicList(Const(list))
    }

    implicit def implicitDynamicList[A](list: List[A]): DynamicList[A] = {
      new DynamicList(Const(list))
    }
  }
}
