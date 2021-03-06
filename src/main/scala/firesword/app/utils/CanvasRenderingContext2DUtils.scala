package firesword.app.utils

import org.scalajs.dom.CanvasRenderingContext2D

object CanvasRenderingContext2DUtils {
  def strokeRoundedRect(
                         ctx: CanvasRenderingContext2D,
                         x: Double,
                         y: Double,
                         width: Double,
                         height: Double,
                         radius: Double,
                       ) {
    ctx.beginPath();
    ctx.moveTo(x + radius, y);
    ctx.lineTo(x + width - radius, y);
    ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
    ctx.lineTo(x + width, y + height - radius);
    ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
    ctx.lineTo(x + radius, y + height);
    ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
    ctx.lineTo(x, y + radius);
    ctx.quadraticCurveTo(x, y, x + radius, y);
    ctx.closePath();

    ctx.stroke();
  }
}
