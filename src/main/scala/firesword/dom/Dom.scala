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

    def div(
             children: List[Widget] = List(),
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

        console.log(s"Setting style: ${is}")
      })

      children.foreach(c => {
        element.appendChild(c.node)
      })

      new Widget(element)
    }
  }
}
