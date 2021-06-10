package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell
import org.scalajs.dom.console

object CellCached {
  class CellCached[A](source: Cell[A]) extends SimpleCell[A] {
    private var cachedValue: Option[A] = None

    private var unsubscribe: Unsubscribe = _

    @behavior
    def sample(): A = cachedValue.getOrElse({
      source.sample()
    })

    private def handle(a: A): Unit = {
      if (!cachedValue.contains(a)) {
        notifyListeners(a)
        cachedValue = Some(a)
      }
    }

    override protected def onStart(): Unit = {
      unsubscribe = source.addListener(handle)
      cachedValue = Some(source.sample())
    }

    override protected def onStop(): Unit = {
      cachedValue = None
      unsubscribe()
      unsubscribe = null
    }
  }
}
