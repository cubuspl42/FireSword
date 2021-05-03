package firesword.frp.cell

import firesword.frp.Frp.behavior
import firesword.frp.SimpleCell.SimpleCell

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object CellFromFuture {
  class CellFromFuture[A, B](
                              future: Future[A],
                              notCompleted: => B,
                              successfullyCompleted: A => B,
                              failed: Throwable => B,
                            ) extends SimpleCell[B] {
    private def fromTry(tryA: Try[A]): B =
      tryA.fold(failed, successfullyCompleted)

    private var _currentState: B =
      future.value.fold(notCompleted)(fromTry)

    @behavior
    def sample(): B = _currentState

    override protected def onStart(): Unit = {
      future.onComplete(tryA => {
        val b = fromTry(tryA)
        notifyListeners(b)
        _currentState = b
      })
    }

    override protected def onStop(): Unit = {
    }
  }
}
