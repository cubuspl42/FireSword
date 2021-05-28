package firesword.frp

import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.{Listener, Unsubscribe, behavior}
import firesword.frp.SourceEventStream.SourceEventStream

import scala.collection.mutable

class Till {
  private val _listeners = new mutable.HashSet[() => Unit]

  private var _wasReached = false

  def wasReached: Boolean = _wasReached

  @behavior
  def on: EventStream[Unit] = new SourceEventStream((h: Listener[Unit]) =>
    addListener(
      () => h(())
    )
  )

  def addListener(h: () => Unit): Unsubscribe = {
    if (!_wasReached) {
      _listeners.addOne(h)

      () => {
        _listeners.remove(h)
      }
    } else () => {}
  }

  protected def markReached(): Unit = {
    if (!_wasReached) {
      _wasReached = true
      _listeners.foreach(l => l())
      _listeners.clear()
    }
  }
}

class TillNext(
                eventStream: EventStream[Any],
                orTill: Till,
              ) extends Till {

  private var unsubscribeStream = eventStream.addListener(_ => handle())

  private var unsubscribeTill = orTill.addListener(handle)

  private def handle(): Unit = {
    markReached()

    unsubscribeStream()
    unsubscribeStream = null

    unsubscribeTill()
    unsubscribeTill = null
  }
}

object Till {
  def end: Till = new Till()
}
