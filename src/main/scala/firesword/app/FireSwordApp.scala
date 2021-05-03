package firesword.app

import firesword.app.EditorView.editorView
import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.DynamicList.implicitDynamicList
import firesword.frp.Frp.implicitConstSome
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyValue
import pako.Pako
import scalacss.StyleA

import scala.language.implicitConversions
import scala.scalajs.js.typedarray.Uint8Array

object FireSwordApp {
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (_: Event) => {
      render()
    })

    Pako.inflate(new Uint8Array(1024))
  }

  private def renderCss(): Unit = {
    import scalacss.DevDefaults._

    val style = document.createElement("style")
    style.setAttribute("type", "text/css")
    style.appendChild(document.createTextNode(MyStyles.render))

    document.head.appendChild(style)
  }

  def render(): Unit = {
    renderCss()

    val root = document.getElementById("root")
    Dom.render(root, rootView())
  }

  def rootView(): Widget = {
    val theDiv = div(
      styleClass = MyStyles.root,
      children = List(
        editorView(),
      ),
    )

    theDiv
  }
}
