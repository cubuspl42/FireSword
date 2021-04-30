package firesword.app

import scalacss.DevDefaults._

import scala.language.postfixOps

object MyStyles extends StyleSheet.Inline {
  import dsl._

  val table: StyleA = style(
    display table,
  )

  val tr: StyleA = style(
    display tableRow,
  )

  val td1: StyleA = style(
    display tableCell,
    width (64 px),
    height (64 px),
    backgroundColor red,
  )

  val td2: StyleA = style(
    display tableCell,
    width (64 px),
    height (64 px),
    backgroundColor black,
  )

  val tdHovered: StyleA = style(
    display tableCell,
    width (64 px),
    height (64 px),
    backgroundColor blue,
  )
}
