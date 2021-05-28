package firesword.app

import firesword.app.EditPlaneDialog.labeledCheckboxInput
import firesword.app.editor.EdPlane.EdPlane
import firesword.app.editor.Editor.Editor
import firesword.app.editor.WorldProperties
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Input, Widget}
import firesword.frp.Cell.Cell
import firesword.frp.MutCell.MutCell


object EditWorldDialog {

  import firesword.frp.DynamicList.Implicits.implicitStatic
  import firesword.frp.Frp.{implicitConst, implicitConstSome}

  private def _labeledInput[A](
                                label: String,
                                input: Widget,
                              ): List[Widget] = {
    List(
      span(s"$label: "),
      input,
    )
  }

  // TODO: Reuse controls across dialogs
  private def _labeledValueInput[A](
                                     createInput: A => Input[A],
                                     label: String,
                                     cell: Cell[A],
                                     set: A => Unit,
                                   ): List[Widget] = {
    val input = createInput(cell.sample())

    input.value.listen(set)

    _labeledInput(label, input)
  }

  private def labeledCheckboxInput[A](
                                       label: String,
                                       cell: MutCell[Boolean],
                                     ): List[Widget] = {
    val input = checkbox(cell.sample())

    input.isChecked.listen(cell.set)

    _labeledInput(label, input)
  }

  private def _labeledIntegerInput(
                                    label: String,
                                    cell: Cell[Int],
                                    set: Int => Unit,
                                  ): List[Widget] = _labeledValueInput(integerInput, label, cell, set)

  private def labeledInput[A](
                               createInput: A => Input[A],
                               label: String,
                               property: MutCell[A],
                             ): List[Widget] = {
    _labeledValueInput(createInput, label, property, property.set)
  }

  private def labeledIntegerInput(
                                   label: String,
                                   property: MutCell[Int],
                                 ): List[Widget] = labeledInput(integerInput, label, property)


  private def labeledTextInput(
                                label: String,
                                property: MutCell[String],
                              ): List[Widget] = labeledInput(textInput, label, property)

  def editWorldDialog(
                       editor: Editor,
                       worldProperties: WorldProperties,
                     ): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStaticSingleton

    def column(children: List[Widget]): Widget = {
      div(
        inlineStyle =
          """
            |    display: grid;
            |    grid-template-columns: max(100px) 150px;
            |    grid-gap: 5px;
            |""".stripMargin,
        children = children,
      )
    }

    val saveButton = button("Save")

    saveButton.onPressed.listen(_ => editor.stopEditing())

    div(
      styleClass = MyStyles.editObjectDialog,
      children = List(
        div(
          styleClass = MyStyles.editObjectDialogRow,
          children = List(
            column(List(
              labeledCheckboxInput("compress", worldProperties.compress),
              labeledCheckboxInput("useZCoords", worldProperties.useZCoords),
              labeledTextInput("name", worldProperties.name),
              labeledTextInput("author", worldProperties.author),
              labeledTextInput("dateCreatedString", worldProperties.dateCreatedString),
              labeledTextInput("rezFilePath", worldProperties.rezFilePath),
              labeledTextInput("imageDir", worldProperties.imageDir),
              labeledTextInput("palRez", worldProperties.palRez),
              labeledIntegerInput("startX", worldProperties.startX),
              labeledIntegerInput("startY", worldProperties.startY),
              labeledTextInput("launchApp", worldProperties.launchApp),
              labeledTextInput("imageSet1", worldProperties.imageSet1),
              labeledTextInput("imageSet2", worldProperties.imageSet2),
              labeledTextInput("imageSet3", worldProperties.imageSet3),
              labeledTextInput("imageSet4", worldProperties.imageSet4),
              labeledTextInput("prefix1", worldProperties.prefix1),
              labeledTextInput("prefix2", worldProperties.prefix2),
              labeledTextInput("prefix3", worldProperties.prefix3),
              labeledTextInput("prefix4", worldProperties.prefix4),
            ).flatten),
          ),
        ),
        div(
          inlineStyle = "align-self: center",
          children = saveButton,
        ),
      )
    )
  }
}
