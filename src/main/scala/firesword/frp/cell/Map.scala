package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.behavior
import firesword.frp.SimpleCell.SimpleCell

object Map {
  class CellMap[A, B](source: Cell[A], f: A => B) extends SimpleCell[B] {
    private var cachedValue: Option[B] = None

    @behavior
    def sample(): B = cachedValue.getOrElse(f(source.sample()))

    private def handle(a: A): Unit = {
      val b = f(a)
      notifyListeners(b)
      cachedValue = Some(b)
    }

    override protected def onStart(): Unit = {
      cachedValue = Some(f(source.sample()))
      source.addListener(handle)
    }

    override protected def onStop(): Unit = {
      source.removeListener(handle)
      cachedValue = None
    }
  }
}
