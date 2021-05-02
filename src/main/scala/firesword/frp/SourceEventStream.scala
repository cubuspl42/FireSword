package firesword.frp

import firesword.frp.Frp.{Listener, Unsubscribe}
import firesword.frp.SimpleEventStream.SimpleEventStream

import scala.language.implicitConversions

object SourceEventStream {
  class SourceEventStream[A](
                              addSourceListener: Listener[A] => Unsubscribe,
                            ) extends SimpleEventStream[A] {

    private var unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      unsubscribe = addSourceListener(notifyListeners)
    }

    override protected def onStop(): Unit = {
      unsubscribe()
      unsubscribe = null
    }
  }
}
