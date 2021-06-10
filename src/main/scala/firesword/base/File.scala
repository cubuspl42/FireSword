package firesword.base

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSName}
import scala.scalajs.js.typedarray.ArrayBuffer

@js.native
trait FileOptions extends js.Object {
  def `type`: String = js.native
}

object FileOptions {
  def apply(mimeType: String): FileOptions =
    js.Dynamic.literal(
      `type` = mimeType,
    ).asInstanceOf[FileOptions]
}

@js.native
@JSGlobal("File")
class File (
            bits: js.Array[ArrayBuffer],
            name: String,
            options: FileOptions,
          ) extends dom.File
