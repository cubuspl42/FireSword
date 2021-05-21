package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.DynamicSet.DynamicSet
import firesword.frp.Frp.Const

import scala.language.implicitConversions

object DynamicList {
  class DynamicList[+A](val content: Cell[List[A]]) {
    def map[B](f: A => B): DynamicList[B] =
      new DynamicList(content.map(_.map(f)))

    def fuseMap[B](f: A => Cell[B]): DynamicList[B] = {
      val clb = content.switchMapC(sa =>
        Cell.traverse(sa.toList, f)
      )

      new DynamicList(content = clb)
    }
  }

  def empty[A](): DynamicList[A] =
    new DynamicList(Const(List()))

  def singleton[A](cell: Cell[A]): DynamicList[A] =
    new DynamicList(cell.map(List(_)))

  def fuse[A](dl: DynamicList[Cell[A]]): DynamicList[A] =
    dl.fuseMap(identity)

  def fuseSome[A](dl: DynamicList[Cell[Option[A]]]): DynamicList[A] =
    new DynamicList(fuse(dl).content.map(_.flatten))

  def fuseSomeStatic[A](l: List[Cell[Option[A]]]): DynamicList[A] =
    new DynamicList(Cell.sequence(l).map(_.flatten))

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
