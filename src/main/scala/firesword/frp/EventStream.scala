package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, action}
import firesword.frp.stream.Hold.StreamHold
import firesword.frp.stream.Map.EventStreamMap

import scala.language.implicitConversions

object EventStream {
  abstract class EventStream[+A] {
    def map[B](f: A => B): EventStream[B] =
      new EventStreamMap(this, f)

    def hold[A1 >: A](initialValue2: A1): Cell[A1] =
      new StreamHold(initialValue2, this)

    @action
    def listen(@action h: A => Unit): Unit

    private[frp] def addListener(h: A => Unit): Unsubscribe

    private[frp] def removeListener(h: A => Unit): Unit
  }
}
