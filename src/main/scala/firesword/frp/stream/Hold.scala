package firesword.frp.stream

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Unsubscribe
import firesword.frp.SimpleCell.SimpleCell
import firesword.frp.SimpleEventStream.SimpleEventStream

object Hold {
  // TODO: Opt-out
  class StreamHold[A](
                     initialValue: A,
                     steps: EventStream[A]
                   ) extends SimpleCell[A] {
    private var _currentValue = initialValue

    override protected def onStart(): Unit = {
    }

    override protected def onStop(): Unit = {
    }

    override def sample(): A = _currentValue

    // TODO: Remove listener
    steps.addListener(a => {
      _currentValue = a
      notifyListeners(a)
    })
  }
}
