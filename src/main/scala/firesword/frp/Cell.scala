package firesword.frp

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.{Unsubscribe, action, behavior}
import firesword.frp.cell.CellCached.CellCached
import firesword.frp.cell.CellFromFuture.CellFromFuture
import firesword.frp.cell.CellSequenceList.CellSequenceList
import firesword.frp.cell.Follow.CellFollowFirst
import firesword.frp.cell.Map2.CellMap2
import firesword.frp.cell.SwitchC.CellSwitchC

import scala.concurrent.Future
import scala.language.implicitConversions

object Cell {
  abstract class Cell[+A] {
    def map[B](f: A => B): Cell[B]

    def switchMapC[B](f: A => Cell[B]): Cell[B] =
      Cell.switchC(this.map(f))

    private[frp] def addListener(h: A => Unit): Unsubscribe

    protected[this] def removeListener(h: A => Unit): Unit


    @action
    def listen(@action h: A => Unit): Unit

    @behavior
    def sample(): A

    def cached(): Cell[A] =
      new CellCached[A](this)
  }

  def map2[A, B, C](ca: Cell[A], cb: Cell[B], f: (A, B) => C): Cell[C] =
    new CellMap2(ca, cb, f).cached()

  def followFirst[A](a: A, f: A => EventStream[A]): Cell[A] =
    new CellFollowFirst[A](a, f)

  def switchC[A](cca: Cell[Cell[A]]): Cell[A] =
    new CellSwitchC(cca).cached()

//  def switchHoldC[A](initialCell: Cell[A], steps: EventStream[Cell[A]]): Cell[A] =
//    switchC(steps.hold(initialCell))

  def fromFuture[A, B](
                        future: Future[A],
                        notCompleted: => B,
                        successfullyCompleted: A => B,
                        failed: Throwable => B,
                      ): Cell[B] =
    new CellFromFuture(
      future = future,
      notCompleted = notCompleted,
      successfullyCompleted = successfullyCompleted,
      failed = failed,
    )

  def sequence[A](lca: List[Cell[A]]): Cell[List[A]] =
    new CellSequenceList[A](lca).cached()

  def traverse[A, B](la: List[A], f: A => Cell[B]): Cell[List[B]] =
    sequence(la.map(f))
}
