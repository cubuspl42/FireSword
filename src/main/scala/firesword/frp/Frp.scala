package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.EventStream.EventStream
import firesword.frp.MutCell.MutCell
import firesword.frp.SimpleEventStream.SimpleEventStream
import firesword.frp.cell.Follow.CellFollowFirst
import firesword.frp.cell.Map.CellMap
import firesword.frp.cell.SwitchC.CellSwitchC

import scala.collection.mutable
import scala.language.implicitConversions

object Frp {
  class behavior() extends scala.annotation.StaticAnnotation

  class action() extends scala.annotation.StaticAnnotation

  type Listener[A] = A => Unit

  type Unsubscribe = () => Unit

  def Const[A](a: A): Cell[A] = new MutCell(a)

  implicit def implicitConst[A](a: A): Cell[A] = Const(a)

  implicit def implicitConstSome[A](a: A): Cell[Option[A]] = Const(Some(a))

  implicit def implicitSome[A](a: A): Option[A] = Some(a)
}
