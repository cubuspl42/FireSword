package firesword.base

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSGlobal("TextDecoder")
class TextDecoder(utfLabel: js.UndefOr[String]) extends js.Object {
  def decode(buffer: Uint8Array): String = js.native
}

object TextDecoder {
  val decoder = new TextDecoder("utf-8")
}
