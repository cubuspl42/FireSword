package firesword.app

import scalacss.DevDefaults._

import scala.List
import scala.language.postfixOps

object MyStyles extends StyleSheet.Inline {

  import dsl._

  val center: StyleA = style(
    height(100 %%),
    display flex,
    alignItems center,
    justifyContent center,
  )

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

  val toolBar: StyleA = style(
    display flex,
    flexDirection row,
    gap(8 px),

    backgroundColor(c"#77777b"),
    height(32 px),
    justifyContent flexEnd,
    alignItems center,
    paddingLeft(8 px),
    paddingRight(8 px),
  )

  val editorView: StyleA = style(
    width(100 %%),
    height(100 %%),

    display flex,
    flexDirection column,

  )

  val worldViewStack: StyleA = style(
    position relative,
    width(100 %%),
    height(100 %%),
  )


  val worldView: StyleA = style(
    position absolute,
    width(100 %%),
    height(100 %%),

    overflow hidden,
  )

  val editObjectDialogWrapper: StyleA = style(
    position absolute,
    width(100 %%),
    height(100 %%),

    pointerEvents none,
    display flex,
    flexDirection column,
    alignItems center,
    justifyContent center,
  )

  val dialogContainer: StyleA = style(
    position absolute,
    width(100 %%),
    height(100 %%),

    pointerEvents none,
    display flex,
    flexDirection column,
    alignItems center,
    justifyContent center,
  )

  val editObjectDialog: StyleA = style(
    padding(20 px),
    gap(20 px),

    pointerEvents auto,

    backgroundColor lightgrey,

    display flex,
    flexDirection column,

  )

  val editObjectDialogRow: StyleA = style(
    display flex,
    flexDirection row,
    justifyContent spaceAround,
    gap(20 px),
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
