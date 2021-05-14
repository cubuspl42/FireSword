package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object CellSequenceList {
  class CellSequenceList[A](source: List[Cell[A]]) extends SimpleCell[List[A]] {
    private var _unsubscribe: Unsubscribe = _

    @behavior
    def sample(): List[A] = source.map(ca => ca.sample())

    override protected def onStart(): Unit = {
      val us = source.zipWithIndex.map {
        case (ca, i) => ca.addListener(a => {
          val updatedList = sample().updated(i, a)
          notifyListeners(updatedList)
        })
      }
      _unsubscribe = () => us.foreach(u => u())
    }

    override protected def onStop(): Unit = {
      _unsubscribe()
      _unsubscribe = null
    }
  }
}
