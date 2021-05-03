package firesword.app

import scalacss.DevDefaults._

import scala.language.postfixOps

object MyStyles extends StyleSheet.Inline {

  import dsl._

  val root: StyleA = style(
    width(100 vw),
    height(100 vh),
    backgroundColor silver,


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

  val editorView: StyleA = style(
    width(100 %%),
    height(100 %%),

    display flex,
    flexDirection column,
  )

  val tilesView: StyleA = style(
    flexGrow(1),
    overflow hidden,
  )

  val tilesRoot: StyleA = style(
    //    position relative,
    //    pointerEvents none,
    //    left(50 %%),
    //    top(50 %%),
  )

  val tilesOrigin: StyleA = style(
    position relative,
    pointerEvents none,
    left(50 %%),
    top(50 %%),
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
