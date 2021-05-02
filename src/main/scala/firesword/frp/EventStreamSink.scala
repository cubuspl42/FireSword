package firesword.frp

import firesword.frp.SimpleEventStream.SimpleEventStream

import scala.language.implicitConversions

object EventStreamSink {
  class EventStreamSink[A]() extends SimpleEventStream[A] {

    def send(a: A): Unit = {
      notifyListeners(a)
    }

    override protected def onStart(): Unit = {
    }

    override protected def onStop(): Unit = {
    }
  }
}
