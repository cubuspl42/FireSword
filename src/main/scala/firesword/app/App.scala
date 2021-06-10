package firesword.app

import firesword.app.editor.Editor
import firesword.app.editor.Editor.Editor
import firesword.app.utils.FileUtils.readAsArrayBuffer
import firesword.frp.Cell.Cell
import firesword.frp.MutCell
import firesword.frp.MutCell.MutCell
import firesword.wwd.Wwd.readWorld
import org.scalajs.dom.console
import org.scalajs.dom.raw.File

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class App {
  private val _editor = new MutCell(Editor.load())

  val editor: Cell[Future[Editor]] = _editor

  def openFile(file: File): Unit = {
    readAsArrayBuffer(file).map(worldBuffer => {
      val editorFuture = Editor.loadBuffer(worldBuffer)
      _editor.set(editorFuture)
    })
  }
}
