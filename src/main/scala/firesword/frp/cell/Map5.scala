package firesword.frp.cell

import firesword.frp.Cell.Cell
import firesword.frp.Frp.{Unsubscribe, behavior}
import firesword.frp.SimpleCell.SimpleCell

object Map5 {
  class CellMap5[A, B, C, D, E, Rv](
                                 ca: Cell[A],
                                 cb: Cell[B],
                                 cc: Cell[C],
                                 cd: Cell[D],
                                 ce: Cell[E],
                                 f: (A, B, C, D, E) => Rv
                               ) extends SimpleCell[Rv] {

    private var unsubscribeA: Unsubscribe = _
    private var unsubscribeB: Unsubscribe = _
    private var unsubscribeC: Unsubscribe = _
    private var unsubscribeD: Unsubscribe = _
    private var unsubscribeE: Unsubscribe = _

    @behavior
    def sample(): Rv = f(ca.sample(), cb.sample(), cc.sample(), cd.sample(), ce.sample())

    override protected def onStart(): Unit = {
      unsubscribeA = ca.addListener(a => {
        notifyListeners(f(a, cb.sample(), cc.sample(), cd.sample(), ce.sample()))
      })

      unsubscribeB = cb.addListener(b => {
        notifyListeners(f(ca.sample(), b, cc.sample(), cd.sample(), ce.sample()))
      })

      unsubscribeC = cc.addListener(c => {
        notifyListeners(f(ca.sample(), cb.sample(), c, cd.sample(), ce.sample()))
      })

      unsubscribeD = cd.addListener(d => {
        notifyListeners(f(ca.sample(), cb.sample(), cc.sample(), d, ce.sample()))
      })

      unsubscribeE = ce.addListener(e => {
        notifyListeners(f(ca.sample(), cb.sample(), cc.sample(), cd.sample(), e))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribeE()
      unsubscribeE = null

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
