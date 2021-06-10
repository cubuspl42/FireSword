package firesword.scalajsdomext

import org.scalajs.dom.raw.HTMLAnchorElement

import scala.language.implicitConversions
import scala.scalajs.js

object HTMLAnchorElementExt {
  @js.native
  trait HTMLAnchorElementExt extends HTMLAnchorElement {
    var download: String = js.native
  }

  implicit def implicitHTMLAnchorElementExt(self: HTMLAnchorElement): HTMLAnchorElementExt =
    self.asInstanceOf[HTMLAnchorElementExt]
}
