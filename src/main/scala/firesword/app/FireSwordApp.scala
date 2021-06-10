package firesword.app

import firesword.app.EditorView.editorView
import firesword.app.editor.Editor.Editor
import firesword.dom.Dom
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.Widget
import firesword.frp.Cell
import org.scalajs.dom._

import scala.concurrent.Future
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

    style.appendChild(document.createTextNode(
      AllStyles.allStyles.map(s => s.render[String]).mkString("\n")
    ))

    document.head.appendChild(style)
  }

  def render(): Unit = {
    renderCss()

    val app = new App()
    val root = document.getElementById("root")

    Dom.render(root, rootView(app))
  }

  def rootView(app: App): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitSingleton

    div(
      children = app.editor.map(editorFutureView(app, _)),
    )
  }

  def editorFutureView(
                        app: App,
                        editorFuture: Future[Editor],
                      ): Widget = {
    import firesword.frp.DynamicList.Implicits.{implicitSingleton, implicitStaticSingleton}
    import firesword.frp.Frp.{implicitConst, implicitConstSome}

    val child = Cell.fromFuture(
      future = editorFuture,
      notCompleted = div(
        styleClass = MyStyles.center,
        children = span("Loading..."),
      ),
      successfullyCompleted = (editor: Editor) =>
        editorView(
          app = app,
          editor = editor,
        ),
      failed = (throwable: Throwable) => {
        println("Error!")
        println(throwable)
        println(throwable.getCause)
        println(throwable.getStackTrace.mkString("Array(", ", ", ")"))
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
