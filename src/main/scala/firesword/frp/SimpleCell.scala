package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, action, behavior}
import firesword.frp.cell.Map.CellMap

import scala.collection.mutable
import scala.language.implicitConversions

object SimpleCell {
  abstract class SimpleCell[+A] extends Cell[A] {
    private[this] val listeners = new mutable.HashSet[A => Unit]

    def map[B](f: A => B): Cell[B] =
      new CellMap[A, B](this, f)

    private[frp] def addListener(h: A => Unit): Unsubscribe = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }

      () => {
        removeListener(h)
      }
    }

    private[frp] def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)
    }

    protected[this] def notifyListeners(a: A): Unit = {
      listeners.foreach(h => h(a))
    }

    // TODO: Ownership
    @action
    def listen(@action h: A => Unit): Unit = {
      addListener(h)
      h(sample())
    }

    protected def onStart(): Unit

    protected def onStop(): Unit

    @behavior
    def sample(): A
  }

}
