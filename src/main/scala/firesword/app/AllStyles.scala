package firesword.app
import scalacss.internal.mutable.StyleSheet

object AllStyles {
  val allStyles: Seq[StyleSheet.Inline] = List(
    MyStyles,
    EditorView.Styles,
    TileModeSidebar.Styles,
  )
}
