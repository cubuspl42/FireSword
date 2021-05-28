package firesword.frp

import firesword.app.editor.EdObject
import firesword.frp.Cell.Cell
import firesword.frp.DynamicList.DynamicList
import firesword.frp.MutCell.MutCell

import scala.collection.mutable

object DynamicSet {
  abstract class DynamicSet[A] {
    def content: Cell[Set[A]]

    // [K <: Comparable[K]]
    def sortedBy(key: A => Cell[Double]): DynamicList[A] = {
      val clt = content.switchMapC(sa =>
        Cell.traverse(
          sa.toList,
          (a: A) => key(a).map(k => (a, k)),
        )
      )

      val cla = clt.map(lt => lt.sortBy(_._2).map(_._1))

      new DynamicList(content = cla)
    }

    def toList(): DynamicList[A] = new DynamicList(content.map(_.toList))
  }

  def of[A](content: Set[A]): DynamicSet[A] =
    new MutDynamicSet[A](content)

  class MutDynamicSet[A](initialContent: Set[A]) extends DynamicSet[A] {

    private val _content = new MutCell(initialContent)

    override val content: Cell[Set[A]] = _content

    def put(value: A): Unit = {
      val oldContent = _content.sample()
      _content.set(oldContent + value)
    }

    def remove(value: A): Unit = {
      val oldContent = _content.sample()
      _content.set(oldContent - value)
    }
  }
}
