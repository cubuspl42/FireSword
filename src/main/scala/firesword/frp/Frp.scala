package firesword.frp

import scala.collection.mutable
import scala.language.implicitConversions

object Frp {
  class behavior() extends scala.annotation.StaticAnnotation

  class action() extends scala.annotation.StaticAnnotation

  class MapCell[A, B](source: Cell[A], f: A => B) extends SimpleCell[B] {
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

  type Listener[A] = A => Unit

  type Unsubscribe = () => Unit

  abstract class EventStream[+A] {
    private[Frp] def addListener(h: A => Unit): Unit

    private[Frp] def removeListener(h: A => Unit): Unit

    @action
    def listen(@action h: A => Unit): Unit
  }

  abstract class SimpleEventStream[A] extends EventStream[A] {
    private val listeners = new mutable.HashSet[A => Unit]

    private[Frp] def addListener(h: A => Unit): Unit = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }
    }

    private[Frp] def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)
    }

    protected def notifyListeners(a: A): Unit = {
      listeners.foreach(h => h(a))
    }

    // TODO: Ownership
    @action
    def listen(@action h: A => Unit): Unit = {
      addListener(h)
    }

    protected def onStart(): Unit

    protected def onStop(): Unit
  }

  class SourceEventStream[A](
                              addListener: Listener[A] => Unsubscribe,
                            ) extends SimpleEventStream[A] {

    private var unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      unsubscribe = addListener(notifyListeners)
    }

    override protected def onStop(): Unit = {
      unsubscribe()
      unsubscribe = null
    }
  }


  abstract class Cell[+A] {

    def map[B](f: A => B): Cell[B]

    private[Frp] def addListener(h: A => Unit): Unit

    private[Frp] def removeListener(h: A => Unit): Unit


    @action
    def listen(@action h: A => Unit): Unit

    @behavior
    def sample(): A
  }

  abstract class SimpleCell[A] extends Cell[A] {
    private val listeners = new mutable.HashSet[A => Unit]

    def map[B](f: A => B): Cell[B] = new MapCell[A, B](this, f)

    private[Frp] def addListener(h: A => Unit): Unit = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }
    }

    private[Frp] def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)
    }

    protected def notifyListeners(a: A): Unit = {
      listeners.foreach(h => h(a))
    }

    // TODO: Ownership
    @action
    def listen(@action h: A => Unit): Unit = {
      addListener(h)
      h(sample())
    }

    protected def onStart(): Unit

    protected def onStop(): Unit

    @behavior
    def sample(): A
  }


  class MutCell[A](initValue: A) extends SimpleCell[A] {
    private var value = initValue

    @action
    def set(a: A): Unit = {
      notifyListeners(a)
      value = a
    }

    @behavior
    override def sample(): A = value

    override protected def onStart(): Unit = ()

    override protected def onStop(): Unit = ()
  }

  def Const[A](a: A): Cell[A] = new MutCell(a)

  implicit def implicitConst[A](a: A): Cell[A] = Const(a)

  implicit def implicitConstSome[A](a: A): Cell[Option[A]] = Const(Some(a))

  implicit def implicitSome[A](a: A): Option[A] = Some(a)
}
