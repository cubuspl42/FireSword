package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object Map3 {
  class CellMap3[A, B, C, D](
                              ca: Cell[A],
                              cb: Cell[B],
                              cc: Cell[C],
                              f: (A, B, C) => D
                            ) extends SimpleCell[D] {

    private var unsubscribeA: Unsubscribe = _
    private var unsubscribeB: Unsubscribe = _
    private var unsubscribeC: Unsubscribe = _

    @behavior
    def sample(): D = f(ca.sample(), cb.sample(), cc.sample())

    override protected def onStart(): Unit = {
      unsubscribeA = ca.addListener(a => {
        notifyListeners(f(a, cb.sample(), cc.sample()))
      })

      unsubscribeB = cb.addListener(b => {
        notifyListeners(f(ca.sample(), b, cc.sample()))
      })

      unsubscribeC = cc.addListener(c => {
        notifyListeners(f(ca.sample(), cb.sample(), c))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribeC()
      unsubscribeC = null

      unsubscribeB()
      unsubscribeB = null

      unsubscribeA()
      unsubscribeA = null
    }
  }
}
