package firesword.app.utils

import scala.scalajs.js.typedarray.Int32Array

class IntMatrixMap(
                    width: Int,
                    height: Int,
                    array: Int32Array,
                  ) {

  def get(i: Int, j: Int): Int = {
    val k = i * width + j
    array.get(k)
  }

  def forEach(h: (Int, Int, Int) => Unit): Unit = {
    for (
      i <- 0 until height;
      j <- 0 until width
    ) {
      h(i, j, get(i, j))
    }
  }
}
