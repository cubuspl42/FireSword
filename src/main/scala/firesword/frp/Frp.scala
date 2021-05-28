package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.MutCell.MutCell

import scala.language.implicitConversions

object Frp {
  class behavior() extends scala.annotation.StaticAnnotation

  class action() extends scala.annotation.StaticAnnotation

  type Listener[A] = A => Unit

  type Unsubscribe = () => Unit

  def Const[A](a: A): Cell[A] = new MutCell(a)

  implicit def implicitConst[A](a: A): Cell[A] = Const(a)

  implicit def implicitConstSome[A](a: A): Cell[Option[A]] = Const(Some(a))

  implicit def implicitMapSome[A](ca: Cell[A]): Cell[Option[A]] = ca.map(Some(_))

  implicit def implicitSome[A](a: A): Option[A] = Some(a)
}
