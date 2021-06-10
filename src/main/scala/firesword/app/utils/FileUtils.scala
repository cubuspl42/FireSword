package firesword.app.utils

import org.scalajs.dom.{File, FileReader}

import scala.scalajs.js.Promise
import scala.scalajs.js.typedarray.ArrayBuffer

object FileUtils {
  def readAsArrayBuffer(file: File) =
    new Promise[ArrayBuffer]((resolve, reject) => {
      val fr = new FileReader()
      fr.onload = _ => {
        resolve(fr.result.asInstanceOf[ArrayBuffer])
      }
      fr.onerror = reject
      fr.readAsArrayBuffer(file)
    }).toFuture
}
