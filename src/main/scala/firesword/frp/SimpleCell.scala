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
      new CellMap[A, B](this, f).cached()

    private[frp] def addListener(h: A => Unit): Unsubscribe = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }

      () => {
        removeListener(h)
      }
    }

    protected[this] def removeListener(h: A => Unit): Unit = {
      val wasThere = listeners.remove(h)
      if (!wasThere) {
        throw new IllegalStateException("Attempted to remove non-existing listener")
      }

      if (listeners.isEmpty) {
        onStop()
      }
    }

    protected[this] def notifyListeners(a: A): Unit = {
      listeners.foreach(h => h(a))
    }

    @action
    def listen(@action h: A => Unit): Unit =
      listenTill(h, Till.end)

    @action
    def listenTill(
                    @action h: A => Unit,
                    till: Till,
                  ): Unit = {
      if (!till.wasReached) {
        val unsubscribe = addListener(h)
        till.addListener(unsubscribe)
        h(sample())
      }
    }

    protected def onStart(): Unit

    protected def onStop(): Unit

    @behavior
    def sample(): A
  }

}
