package firesword.dom

import firesword.app.Geometry.Vec2d
import firesword.frp.Cell.Cell
import firesword.frp.{DynamicList, Till}
import firesword.frp.DynamicList.DynamicList
import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Const
import firesword.frp.SourceEventStream.SourceEventStream
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Element
import org.scalajs.dom.raw.{HTMLButtonElement, HTMLElement, HTMLInputElement, HTMLOptionElement, HTMLSelectElement}
import scalacss.StyleA

object Dom {
  private def clearElement(element: Element): Unit = {
    element.innerHTML = ""
  }

  private def elementEventStream[E <: Event](element: Node, eventType: String): EventStream[E] =
    new SourceEventStream[E](listener_ => {
      element.addEventListener(eventType, listener_)
      () => element.removeEventListener(eventType, listener_)
    })

  class MouseGesture(
                      val clientPos: Cell[Vec2d],
                      val tillEnd: Till,
                    )

  object MouseGesture {
    def startDrag(
                   element: Widget,
                   event: MouseEvent,
                   tillAbort: Till,
                 ): MouseGesture = start(
      element = element,
      onEnd = element.onMouseUp,
      event = event, tillAbort = tillAbort,
    )

    def startHover(
                    element: Widget,
                    event: MouseEvent,
                    tillAbort: Till,
                  ): MouseGesture = start(
      element = element,
      onEnd = element.onMouseLeave,
      event = event, tillAbort = tillAbort,
    )

    private def start(
                       element: Widget,
                       onEnd: EventStream[MouseEvent],
                       event: MouseEvent,
                       tillAbort: Till,
                     ): MouseGesture = {
      val tillEnd = onEnd.tillNext(tillAbort)

      val clientPos = element.onMouseMove.holdTill(event, tillEnd)
        .map(e => Vec2d(e.clientX, e.clientY))

      new MouseGesture(
        clientPos = clientPos,
        tillEnd = tillEnd,
      )
    }
  }


  class Widget(val node: Element) {

    lazy val onMouseDown: EventStream[MouseEvent] =
      elementEventStream[MouseEvent](node, "mousedown")

    lazy val onMouseDrag: EventStream[MouseGesture] =
      ???

    lazy val onMouseHover: EventStream[MouseGesture] =
      elementEventStream[MouseEvent](node, "mouseenter").map(e =>
        MouseGesture.startHover(
          element = this,
          event = e,
          tillAbort = Till.end,
        ),
      )

    lazy val onMouseUp: EventStream[MouseEvent] =
      elementEventStream[MouseEvent](node, "mouseup")

    lazy val onMouseLeave: EventStream[MouseEvent] =
      elementEventStream[MouseEvent](node, "mouseleave")

    lazy val onPointerDown: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointerdown")

    lazy val onPointerUp: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointerup")

    lazy val onPointerMove: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointermove")

    lazy val onMouseMove: EventStream[MouseEvent] =
      elementEventStream[MouseEvent](node, "mousemove")

    def calculateRelativePosition(clientV: Vec2d): Vec2d = {
      val rect = node.getBoundingClientRect()
      val x = clientV.x - rect.left
      val y = clientV.y - rect.top
      Vec2d(x, y)
    }

    def calculateRelativePosition(e: MouseEvent): Vec2d =
      calculateRelativePosition(Vec2d(e.clientX, e.clientY))
  }


  abstract class Input[A](element: HTMLInputElement) extends Widget(element) {
    lazy val value: Cell[A] = {
      val onChange = elementEventStream[Event](element, "change")

      onChange.map(e => element.value)
        .hold(element.value)
        .map(parseString)
    }

    protected def parseString(s: String): A
  }

  class IntegerInput(element: HTMLInputElement) extends Input[Int](element) {
    override protected def parseString(s: String): Int = s.toInt
  }

  class TextInput(element: HTMLInputElement) extends Input[String](element) {
    override protected def parseString(s: String): String = s
  }

  class Checkbox(element: HTMLInputElement) extends Widget(element) {
    lazy val isChecked: Cell[Boolean] = {
      val onChange = elementEventStream[Event](element, "change")

      onChange.map(e => element.checked)
        .hold(element.checked)
    }
  }

  class Button(val element: HTMLButtonElement) extends Widget(element) {
    lazy val onPressed: EventStream[MouseEvent] =
      elementEventStream[MouseEvent](node, "click")
  }

  class Select[A](
                   val element: HTMLSelectElement,
                   options: DynamicList[A],
                 ) extends Widget(element) {
    lazy val value: Cell[A] = {
      val onChange = elementEventStream[Event](element, "change")
      onChange.map(e => element.value).hold(element.value)
        .map(iStr => {
          val i = iStr.toInt
          val o = options.content.sample()(i)
          o
        })
    }
  }

  def render(target: dom.Node, widget: Widget): Unit = {
    target.appendChild(widget.node)
  }

  def widgetList(widgets: Cell[Option[Widget]]*): DynamicList[Widget] = {
    val list = widgets.toList;
    DynamicList.fuseSomeStatic(list)
  }

  object Tag {
    def p(text: Cell[String]): Widget = {
      val element = document.createElement("p").asInstanceOf[Element]

      // TODO: Unlisten
      text.listen(t => {
        element.textContent = t
      })

      new Widget(element)
    }

    def span(text: Cell[String]): Widget = {
      val element = document.createElement("span").asInstanceOf[Element]

      // TODO: Unlisten
      text.listen(t => {
        element.textContent = t
      })

      new Widget(element)
    }

    def div(
             children: DynamicList[Widget] = DynamicList.empty(),
             styleClass: Cell[Option[StyleA]] = Const(None),
             inlineStyle: Cell[String] = Const(""),
           ): Widget = {
      val element = document.createElement("div").asInstanceOf[Element]
      val classList = element.classList

      styleClass.listen(sOpt => {
        classList.remove(classList.item(0))
        sOpt.foreach(s => {
          classList.add(s.htmlClass)
        })
      })

      inlineStyle.listen(is => {
        element.style = is
      })

      children.content.listen(children => {
        clearElement(element)
        children.foreach(c => {
          element.appendChild(c.node)
        })
      })

      new Widget(element)
    }

    def select[A](
                   options: DynamicList[A],
                   initialSelected: A,
                   toString: A => String,
                 ): Select[A] = {


      //Create and append select list
      val element = document.createElement("select").asInstanceOf[HTMLSelectElement];
      //      selectList.id = "mySelect";

      options.content.listen(options => {
        clearElement(element)
        options.zipWithIndex foreach { case (o, i) =>
          val option = document.createElement("option").asInstanceOf[HTMLOptionElement]

          if (o == initialSelected) {
            option.selected = true
          }

          option.setAttribute("value", i.toString)
          option.textContent = toString(o)
          element.appendChild(option)
        }
      })

      new Select(element, options)
    }

    def integerInput(initialValue: Int): IntegerInput = {
      val element = document.createElement("input").asInstanceOf[HTMLInputElement]
      element.setAttribute("type", "number");
      element.setAttribute("value", initialValue.toString)

      new IntegerInput(element)
    }

    def textInput(initialValue: String): TextInput = {
      val element = document.createElement("input").asInstanceOf[HTMLInputElement]
      element.setAttribute("value", initialValue)

      new TextInput(element)
    }

    def checkbox(initialChecked: Boolean): Checkbox = {
      val element = document.createElement("input").asInstanceOf[HTMLInputElement]
      element.setAttribute("type", "checkbox");
      element.checked = initialChecked

      new Checkbox(element)
    }

    def button(
                text: String): Button = {
      val element = document.createElement("button").asInstanceOf[HTMLButtonElement]
      element.innerText = text
      new Button(element)
    }

    def singletonDiv(
                      child: Cell[Widget],
                    ): Widget = {
      val element = document.createElement("div").asInstanceOf[Element]

      child.listen(c => {
        clearElement(element)
        element.appendChild(c.node)
      })

      new Widget(element)
    }
  }
}
