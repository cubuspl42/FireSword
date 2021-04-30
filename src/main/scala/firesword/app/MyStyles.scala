package firesword.app

import scalacss.DevDefaults._

import scala.language.postfixOps

object MyStyles extends StyleSheet.Inline {

  import dsl._

  val root: StyleA = style(
    width(100 vw),
    height(100 vh),
    backgroundColor silver,


    display flex,
    flexDirection column,
  )

  val table: StyleA = style(
    display table,
  )

  val tr: StyleA = style(
    display tableRow,
  )

  val td1: StyleA = style(
    display tableCell,
    width(64 px),
    height(64 px),
    backgroundColor red,
  )

  val td2: StyleA = style(
    display tableCell,
    width(64 px),
    height(64 px),
    backgroundColor black,
  )

  val tdHovered: StyleA = style(
    display tableCell,
    width(64 px),
    height(64 px),
    backgroundColor blue,
  )

  val tilesView: StyleA = style(
    flexGrow(1),
    overflow hidden,
  )

  val tilesRoot: StyleA = style(
    position relative,
    pointerEvents none,
  )

  val tileFragment1: StyleA = style(
    display flex,
    alignItems center,
    justifyContent center,

    position absolute,
    width(64 px),
    height(64 px),
    backgroundColor lightgrey,
  )

  val tileFragment2: StyleA = style(
    display flex,
    alignItems center,
    justifyContent center,

    position absolute,
    width(64 px),
    height(64 px),
    backgroundColor darkgrey,
  )

  val tileId: StyleA = style(
    display flex,

  )

}
