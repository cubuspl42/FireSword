package firesword.frp

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.{Unsubscribe, action}

import scala.collection.mutable
import scala.language.implicitConversions

object SimpleEventStream {
  abstract class SimpleEventStream[A] extends EventStream[A] {
    private val listeners = new mutable.HashSet[A => Unit]

    override def addListener(h: A => Unit): Unsubscribe = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }

      () => {
        removeListener(h)
      }
    }

    override def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)

      if (listeners.isEmpty) {
        onStop()
      }
    }

    protected def notifyListeners(a: A): Unit = {
      listeners.foreach(h => h(a))
    }

    // TODO: Ownership
    @action
    def listen(@action h: A => Unit): Unit = {
      addListener(h)
    }

    protected def onStart(): Unit

    protected def onStop(): Unit
  }
}
