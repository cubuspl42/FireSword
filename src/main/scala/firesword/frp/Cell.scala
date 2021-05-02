package firesword.frp

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.{Unsubscribe, action, behavior}
import firesword.frp.cell.Follow.CellFollowFirst
import firesword.frp.cell.SwitchC.CellSwitchC

import scala.language.implicitConversions

object Cell {
  abstract class Cell[+A] {
    def map[B](f: A => B): Cell[B]

    def switchMapC[B](f: A => Cell[B]): Cell[B] =
      Cell.switchC(this.map(f))

    private[frp] def addListener(h: A => Unit): Unsubscribe

    private[frp] def removeListener(h: A => Unit): Unit


    @action
    def listen(@action h: A => Unit): Unit

    @behavior
    def sample(): A
  }

  def map2[A, B, C](ca: Cell[A], cb: Cell[B], f: (A, B) => C): Cell[C] =
    ca.switchMapC(a =>
      cb.map(b =>
        f(a, b),
      ),
    )

  def followFirst[A](a: A, f: A => EventStream[A]): Cell[A] =
    new CellFollowFirst[A](a, f)

  def switchC[A](cca: Cell[Cell[A]]): Cell[A] =
    new CellSwitchC(cca)

  def switchHoldC[A](initialCell: Cell[A], steps: EventStream[Cell[A]]): Cell[A] =
    switchC(steps.hold(initialCell))
}
