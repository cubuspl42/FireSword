package firesword.frp

import firesword.frp.Cell.Cell
import firesword.frp.DynamicList.DynamicList
import firesword.frp.MutCell.MutCell

object DynamicMap {
  abstract class DynamicMap[K, V] {

    def content: Cell[Map[K, V]]

    def toList: DynamicList[(K, V)] =
      new DynamicList(content.map(_.toList))
  }

  class MutDynamicMap[K, V](initialContent: Map[K, V]) extends DynamicMap[K, V] {
    private val _content = new MutCell(initialContent)

    override val content: Cell[Map[K, V]] = _content

    def put(key: K, value: V): Unit = {
      val oldContent = _content.sample()
      _content.set(oldContent + (key -> value))
    }
  }
}
