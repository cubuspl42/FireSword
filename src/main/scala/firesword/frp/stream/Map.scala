package firesword.frp.stream

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Unsubscribe
import firesword.frp.SimpleEventStream.SimpleEventStream

object Map {
  class EventStreamMap[A, B](
                              source: EventStream[A],
                              f: A => B,
                            ) extends SimpleEventStream[B] {
    private var unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      unsubscribe = source.addListener(a => {
        notifyListeners(f(a))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribe()
      unsubscribe = null
    }
  }
}
