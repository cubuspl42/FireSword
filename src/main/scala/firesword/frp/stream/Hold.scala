package firesword.frp.stream

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Unsubscribe
import firesword.frp.SimpleCell.SimpleCell
import firesword.frp.SimpleEventStream.SimpleEventStream
import firesword.frp.Till

object Hold {
  class StreamHold[A](
                       initialValue: A,
                       steps: EventStream[A],
                       till: Till,
                     ) extends SimpleCell[A] {
    private var _currentValue = initialValue

    override protected def onStart(): Unit = {
    }

    override protected def onStop(): Unit = {
    }

    override def sample(): A = _currentValue

    if (!till.wasReached) {
      val unsubscribe = steps.addListener(a => {
        _currentValue = a
        notifyListeners(a)
      })

      till.addListener(unsubscribe)
    }
  }
}
