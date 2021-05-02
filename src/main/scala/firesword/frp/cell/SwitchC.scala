package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.Unsubscribe
import firesword.frp.SimpleCell.SimpleCell

object SwitchC {
  class CellSwitchC[A](source: Cell[Cell[A]]) extends SimpleCell[A] {
    private var _unsubscribeOuter: Unsubscribe = _

    private var _unsubscribeInner: Unsubscribe = _

    override protected def onStart(): Unit = {
      def addInnerListener(inner: Cell[A]): Unit = {
        _unsubscribeInner = inner.addListener(notifyListeners)
      }

      def reAddInnerListener(inner: Cell[A]): Unit = {
        _unsubscribeInner()
        addInnerListener(inner)
      }

      _unsubscribeOuter = source.addListener(inner => {
        notifyListeners(inner.sample())
        reAddInnerListener(inner)
      })


      addInnerListener(source.sample())
    }

    override protected def onStop(): Unit = {
      _unsubscribeInner()
      _unsubscribeInner = null

      _unsubscribeOuter()
      _unsubscribeOuter = null
    }

    override def sample(): A =
      source.sample().sample()
  }

}
