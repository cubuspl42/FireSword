package firesword.app.utils

import firesword.frp.Cell.Cell
import firesword.frp.MutCell.MutCell

import scala.scalajs.js.typedarray.Int32Array

trait DynamicIntMatrix {
  def get(i: Int, j: Int): Int

  def forEach(h: (Int, Int, Int) => Unit): Unit

  def marker: Cell[Unit]
}

class MutDynamicIntMatrix(
                           width: Int,
                           height: Int,
                           array: Int32Array,
                         ) extends DynamicIntMatrix {

  private val _marker = new MutCell(())

  override def marker: Cell[Unit] = _marker

  def get(i: Int, j: Int): Int = {
    val k = i * width + j
    array.get(k)
  }

  def set(i: Int, j: Int, value: Int): Unit = {
    val k = i * width + j
    if (array.get(k) != value) {
      array.set(k, value)
      _marker.set(())
    }
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
