package firesword.app.utils

import scala.scalajs.js.typedarray.Int32Array

trait IntMatrixMap {
  def get(i: Int, j: Int): Int

  def forEach(h: (Int, Int, Int) => Unit): Unit
}

class MutIntMatrixMap(
                       width: Int,
                       height: Int,
                       array: Int32Array,
                     ) extends IntMatrixMap {
  def get(i: Int, j: Int): Int = {
    val k = i * width + j
    array.get(k)
  }

  def set(i: Int, j: Int, value: Int): Unit = {
    val k = i * width + j
    array.set(k, value)
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
