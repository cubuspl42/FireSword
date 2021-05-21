package firesword.frp

import firesword.frp.Frp.{action, behavior}
import firesword.frp.SimpleCell.SimpleCell

import scala.language.implicitConversions

object MutCell {
  class MutCell[A](initValue: A) extends SimpleCell[A] {
    private var value = initValue

    @action
    def set(a: A): Unit = {
      value = a
      notifyListeners(a)
    }

    @action
    def update(f: A => A): Unit = {
      val oldValue = this.sample()
      this.set(f(oldValue))
    }

    @behavior
    override def sample(): A = value

    override protected def onStart(): Unit = ()

    override protected def onStop(): Unit = ()
  }
}
