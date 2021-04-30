package firesword.dom

import org.scalajs.dom
import org.scalajs.dom.{Node, document}

object Dom {
  class Widget(val node: Node)

  def render(target: dom.Node, widget: Widget): Unit = {
    target.appendChild(widget.node)
  }

  object Tag {
    def p(text: String): Widget = {
      val element = document.createElement("p")
      element.textContent = text
      new Widget(element)
    }

    def div(children: List[Widget]): Widget = {
      val element = document.createElement("div")
      children.foreach(c => {
        element.appendChild(c.node)
      })
      new Widget(element)
    }
  }
}
