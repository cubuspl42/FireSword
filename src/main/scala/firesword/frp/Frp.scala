package firesword.frp

import firesword.frp.cell.Follow.CellFollowFirst

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

  class CellSwitchC[A](source: Cell[Cell[A]]) extends SimpleCell[A] {
    private var _unsubscribeOuter: Unsubscribe = _

    private var _unsubscribeInner: Unsubscribe = _

    override protected def onStart(): Unit = {
      def addInnerListener(inner: Cell[A]): Unit = {
        _unsubscribeInner = inner.addListener(notifyListeners)
      }

      def reAddInnerListener(inner: Cell[A]): Unit = {
        _unsubscribeInner()
        addInnerListener(inner)
      }

      _unsubscribeOuter = source.addListener(inner => {
        notifyListeners(inner.sample())
        reAddInnerListener(inner)
      })


      addInnerListener(source.sample())
    }

    override protected def onStop(): Unit = {
      _unsubscribeInner()
      _unsubscribeInner = null

      _unsubscribeOuter()
      _unsubscribeOuter = null
    }

    override def sample(): A =
      source.sample().sample()
  }

  type Listener[A] = A => Unit

  type Unsubscribe = () => Unit

  abstract class EventStream[+A] {
    def map[B](f: A => B): EventStream[B] =
      new MapEventStream(this, f)

    def hold[A1 >: A](initialValue2: A1): Cell[A1] =
      new HoldCell(initialValue2, this)

    @action
    def listen(@action h: A => Unit): Unit

    private[frp] def addListener(h: A => Unit): Unsubscribe

    private[Frp] def removeListener(h: A => Unit): Unit


  }

  abstract class SimpleEventStream[A] extends EventStream[A] {
    private val listeners = new mutable.HashSet[A => Unit]

    override def addListener(h: A => Unit): Unsubscribe = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }

      () => {
        removeListener(h)
      }
    }

    override def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)

      if (listeners.isEmpty) {
        onStop()
      }
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


  class EventStreamSink[A](

                          ) extends SimpleEventStream[A] {

    def send(a: A): Unit = {
      notifyListeners(a)
    }

    override protected def onStart(): Unit = {
    }

    override protected def onStop(): Unit = {
    }
  }


  class SourceEventStream[A](
                              addSourceListener: Listener[A] => Unsubscribe,
                            ) extends SimpleEventStream[A] {

    private var unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      unsubscribe = addSourceListener(notifyListeners)
    }

    override protected def onStop(): Unit = {
      unsubscribe()
      unsubscribe = null
    }
  }

  class MapEventStream[A, B](
                              source: EventStream[A],
                              f: A => B,
                            ) extends SimpleEventStream[B] {
    private var unsubscribe: Unsubscribe = _

    override protected def onStart(): Unit = {
      unsubscribe = source.addListener(a => {
        notifyListeners(f(a))
      })
    }

    override protected def onStop(): Unit = {
      unsubscribe()
      unsubscribe = null
    }
  }


  abstract class Cell[+A] {

    def map[B](f: A => B): Cell[B]

    def switchMapC[B](f: A => Cell[B]): Cell[B] =
      Cell.switchC(this.map(f))

    private[Frp] def addListener(h: A => Unit): Unsubscribe

    private[Frp] def removeListener(h: A => Unit): Unit


    @action
    def listen(@action h: A => Unit): Unit

    @behavior
    def sample(): A
  }

  object Cell {

    def followFirst[A](a: A, f: A => EventStream[A]): Cell[A] =
      new CellFollowFirst[A](a, f)


    def switchC[A](cca: Cell[Cell[A]]): Cell[A] = new CellSwitchC(cca)

    def switchHoldC[A](initialCell: Cell[A], steps: EventStream[Cell[A]]): Cell[A] =
      switchC(steps.hold(initialCell))
  }

  abstract class SimpleCell[+A] extends Cell[A] {
    private[this] val listeners = new mutable.HashSet[A => Unit]

    def map[B](f: A => B): Cell[B] = new MapCell[A, B](this, f)

    private[Frp] def addListener(h: A => Unit): Unsubscribe = {
      listeners.addOne(h)

      if (listeners.size == 1) {
        onStart()
      }

      () => {
        removeListener(h)
      }
    }

    private[Frp] def removeListener(h: A => Unit): Unit = {
      listeners.remove(h)
    }

    protected[this] def notifyListeners(a: A): Unit = {
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

  class HoldCell[A](
                     initialValue: A,
                     steps: EventStream[A]
                   ) extends SimpleCell[A] {
    private var _currentValue = initialValue

    override protected def onStart(): Unit = {
    }

    override protected def onStop(): Unit = {
    }

    override def sample(): A = _currentValue

    // TODO: Remove listener
    steps.addListener(a => {
      notifyListeners(a)
      _currentValue = a
    })
  }

  object HoldCell {

  }


  def Const[A](a: A): Cell[A] = new MutCell(a)

  implicit def implicitConst[A](a: A): Cell[A] = Const(a)

  implicit def implicitConstSome[A](a: A): Cell[Option[A]] = Const(Some(a))

  implicit def implicitSome[A](a: A): Option[A] = Some(a)
}
