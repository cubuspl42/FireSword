package firesword.dom

import firesword.frp.Frp.{Cell, Const, EventStream, SourceEventStream}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Element
import scalacss.StyleA

object Dom {
  private def elementEventStream[E <: Event](element: Element, eventType: String): EventStream[E] =
    new SourceEventStream[E](listener => {
      element.addEventListener(eventType, listener)
      () => element.removeEventListener(eventType, listener)
    })

  class Widget(val node: Element) {
    lazy val onPointerDown: EventStream[PointerEvent] = elementEventStream[PointerEvent](node, "pointerdown")
  }

  def render(target: dom.Node, widget: Widget): Unit = {
    target.appendChild(widget.node)
  }

  object Tag {
    def p(text: Cell[String]): Widget = {
      val element = document.createElement("p").asInstanceOf[Element]

      // TODO: Unlisten
      text.listen(t => {
        console.log(t)
        element.textContent = t
      })

      new Widget(element)
    }

    def div(style: Cell[Option[StyleA]], children: List[Widget]): Widget = {
      val element = document.createElement("div").asInstanceOf[Element]
      val classList = element.classList

      style.listen(sOpt => {
        classList.remove(classList.item(0))
        sOpt.foreach(s => {
          classList.add(s.htmlClass)
        })
      })

      children.foreach(c => {
        element.appendChild(c.node)
      })

      new Widget(element)
    }

    def div(style: StyleA, children: List[Widget]): Widget = {
      div(Const(Some(style)), children)
    }

    def div(children: List[Widget]): Widget =
      div(Const(None), children)
  }
}
