package firesword.frp.cell

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Unsubscribe
import firesword.frp.SimpleCell.SimpleCell

object Follow {
  // TODO: Opt-out
  class CellFollowFirst[A](initialValue: A, f: A => EventStream[A]) extends SimpleCell[A] {
    private var _currentValue = initialValue

    private var _unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      def addStreamListener(next: EventStream[A]): Unit = {
        _unsubscribe = next.addListener(a => {
          notifyListeners(a)

          _unsubscribe()
          _currentValue = a
          addStreamListener(f(a))
        })
      }

      _currentValue = initialValue
      addStreamListener(f(initialValue))
    }

    override protected def onStop(): Unit = {
      _unsubscribe()
      _unsubscribe = null
    }

    override def sample(): A = _currentValue
  }
}
