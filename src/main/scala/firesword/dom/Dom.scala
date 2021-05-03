package firesword.dom

import firesword.frp.Cell.Cell
import firesword.frp.DynamicList
import firesword.frp.DynamicList.DynamicList
import firesword.frp.EventStream.EventStream
import firesword.frp.Frp.Const
import firesword.frp.SourceEventStream.SourceEventStream
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Element
import scalacss.StyleA

object Dom {
  private def clearElement(element: Element): Unit = {
    element.innerHTML = ""
  }

  private def elementEventStream[E <: Event](element: Element, eventType: String): EventStream[E] =
    new SourceEventStream[E](listener_ => {
      element.addEventListener(eventType, listener_)
      () => element.removeEventListener(eventType, listener_)
    })

  class Widget(val node: Element) {

    lazy val onPointerDown: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointerdown")


    lazy val onPointerUp: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointerup")


    lazy val onPointerMove: EventStream[PointerEvent] =
      elementEventStream[PointerEvent](node, "pointermove")
  }

  def render(target: dom.Node, widget: Widget): Unit = {
    target.appendChild(widget.node)
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
