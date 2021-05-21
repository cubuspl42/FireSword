package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object Map4 {
  class CellMap4[A, B, C, D, E](
                                 ca: Cell[A],
                                 cb: Cell[B],
                                 cc: Cell[C],
                                 cd: Cell[D],
                                 f: (A, B, C, D) => E
                               ) extends SimpleCell[E] {

    private var unsubscribeA: Unsubscribe = _
    private var unsubscribeB: Unsubscribe = _
    private var unsubscribeC: Unsubscribe = _
    private var unsubscribeD: Unsubscribe = _

    @behavior
    def sample(): E = f(ca.sample(), cb.sample(), cc.sample(), cd.sample())

    override protected def onStart(): Unit = {
      unsubscribeA = ca.addListener(a => {
        notifyListeners(f(a, cb.sample(), cc.sample(), cd.sample()))
      })

      unsubscribeB = cb.addListener(b => {
        notifyListeners(f(ca.sample(), b, cc.sample(), cd.sample()))
      })

      unsubscribeC = cc.addListener(c => {
        notifyListeners(f(ca.sample(), cb.sample(), c, cd.sample()))
      })

      unsubscribeD = cd.addListener(d => {
        notifyListeners(f(ca.sample(), cb.sample(), cc.sample(), d))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribeD()
      unsubscribeD = null

      unsubscribeC()
      unsubscribeC = null

      unsubscribeB()
      unsubscribeB = null

      unsubscribeA()
      unsubscribeA = null
    }
  }
}
