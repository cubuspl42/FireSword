package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object Map2 {
  class CellMap2[A, B, C](
                           ca: Cell[A],
                           cb: Cell[B],
                           f: (A, B) => C
                         ) extends SimpleCell[C] {

    private var unsubscribeA: Unsubscribe = _
    private var unsubscribeB: Unsubscribe = _

    @behavior
    def sample(): C = f(ca.sample(), cb.sample())

    override protected def onStart(): Unit = {
      unsubscribeA = ca.addListener(a => {
        notifyListeners(f(a, cb.sample()))
      })

      unsubscribeB = cb.addListener(b => {
        notifyListeners(f(ca.sample(), b))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribeB()
      unsubscribeB = null

      unsubscribeA()
      unsubscribeA = null
    }
  }
}
