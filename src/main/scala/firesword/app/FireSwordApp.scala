package firesword.app

import firesword.app.EditorView.editorView
import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import org.scalajs.dom._

import scala.language.implicitConversions

object FireSwordApp {
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (_: Event) => {
      render()
    })
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
    import firesword.frp.DynamicList.Implicits.{implicitSingleton, implicitStaticSingleton}
    import firesword.frp.Frp.{implicitConst, implicitConstSome}

    val editorFuture = Editor.load()

    val child = Cell.fromFuture(
      future = editorFuture,
      notCompleted = div(
        styleClass = MyStyles.center,
        children = span("Loading..."),
      ),
      successfullyCompleted = editorView,
      failed = (throwable: Throwable) => {
        println("Error!")
        println(throwable)
        println(throwable.getCause)
        div(
          styleClass = MyStyles.center,
          children = span("Error"),
        )
      },
    )

    val theDiv = div(
      styleClass = MyStyles.root,
      children = child,
    )

    theDiv
  }
}
