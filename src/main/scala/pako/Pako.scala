package pako

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("pako", JSImport.Namespace)
object Pako extends js.Object {
  def inflate(data: Uint8Array): Uint8Array = js.native

  // function deflate(data: Data, options?: DeflateFunctionOptions): Uint8Array;
  def deflate(data: Uint8Array): Uint8Array = js.native
}
