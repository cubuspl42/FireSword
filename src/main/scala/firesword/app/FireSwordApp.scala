package firesword.app

import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import org.scalajs.dom.document
import org.scalajs.dom.Event

object FireSwordApp {
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (_: Event) => {
      render()
    })
  }

  def render(): Unit = {
    val root = document.getElementById("root")
    Dom.render(root, rootView())
  }

  def rootView(): Widget =
    div(List(
      p("1"),
    ))
}
