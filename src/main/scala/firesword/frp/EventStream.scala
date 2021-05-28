package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, action, behavior}
import firesword.frp.stream.Hold.StreamHold
import firesword.frp.stream.Map.EventStreamMap

import scala.language.implicitConversions

object EventStream {
  abstract class EventStream[+A] {
    def map[B](f: A => B): EventStream[B] =
      new EventStreamMap(this, f)

    def hold[A1 >: A](initialValue: A1): Cell[A1] =
      new StreamHold(initialValue, this, Till.end)

    def holdTill[A1 >: A](initialValue: A1, till: Till): Cell[A1] =
      new StreamHold(initialValue, this, till)

    @behavior
    def tillNext(orTill: Till): Till = new TillNext(this, orTill)

    @action
    def listen(@action h: A => Unit): Unit

    private[frp] def addListener(h: A => Unit): Unsubscribe

    private[frp] def removeListener(h: A => Unit): Unit
  }
}
