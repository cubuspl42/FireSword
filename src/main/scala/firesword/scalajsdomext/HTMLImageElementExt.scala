package firesword.scalajsdomext

import org.scalajs.dom.raw.{Event, HTMLImageElement}

import scala.language.implicitConversions
import scala.scalajs.js

object HTMLImageElementExt {
  @js.native
  trait HTMLImageElementExt extends HTMLImageElement {
    var onerror: js.Function1[Any, _] = js.native
  }

  implicit def implicitHTMLImageElementExt(self: HTMLImageElement): HTMLImageElementExt =
    self.asInstanceOf[HTMLImageElementExt]
}
