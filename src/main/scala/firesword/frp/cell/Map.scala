package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object Map {
  class CellMap[A, B](source: Cell[A], f: A => B) extends SimpleCell[B] {
    private var cachedValue: Option[B] = None

    private var unsubscribe: Unsubscribe = _


    @behavior
    def sample(): B = cachedValue.getOrElse(f(source.sample()))

    private def handle(a: A): Unit = {
      val b = f(a)
      notifyListeners(b)
      cachedValue = Some(b)
    }

    override protected def onStart(): Unit = {
      unsubscribe = source.addListener(handle)
      cachedValue = Some(f(source.sample()))
    }

    override protected def onStop(): Unit = {
      cachedValue = None
      unsubscribe()
      unsubscribe = null
    }
  }
}
