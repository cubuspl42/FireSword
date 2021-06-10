package firesword.app

import firesword.app.EditPlaneDialog.labeledCheckboxInput
import firesword.app.editor.EdObject.EdObject
import firesword.app.editor.Editor.Editor
import firesword.dom.Dom.Tag._
import firesword.dom.Dom.{Input, Widget}
import firesword.frp.Cell.Cell
import firesword.frp.MutCell.MutCell


object EditObjectDialog {

  import firesword.frp.DynamicList.Implicits.implicitStatic
  import firesword.frp.Frp.{implicitConst, implicitConstSome}

  private def _labeledInput[A](
                                createInput: A => Input[A],
                                label: String,
                                cell: Cell[A],
                                set: A => Unit,
                              ): List[Widget] = {
    val input = createInput(cell.sample())

    input.value.listen(set)

    List(
      span(s"$label: "),
      input,
    )
  }

  private def _labeledIntegerInput(
                                    label: String,
                                    cell: Cell[Int],
                                    set: Int => Unit,
                                  ): List[Widget] = _labeledInput(integerInput, label, cell, set)

  private def labeledInput[A](
                               createInput: A => Input[A],
                               label: String,
                               property: MutCell[A],
                             ): List[Widget] = {
    _labeledInput(createInput, label, property, property.set)
  }

  private def labeledIntegerInput(
                                   label: String,
                                   property: MutCell[Int],
                                 ): List[Widget] = labeledInput(integerInput, label, property)


  private def labeledTextInput(
                                label: String,
                                property: MutCell[String],
                              ): List[Widget] = labeledInput(textInput, label, property)

  def editObjectDialog(
                        editor: Editor,
                        edObject: EdObject,
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

              labeledIntegerInput("id", edObject.id),

              labeledTextInput("name", edObject.name),
              labeledTextInput("logic", edObject.logic),
              labeledTextInput("imageSet", edObject.imageSet),
              labeledTextInput("animation", edObject.animation),

              _labeledIntegerInput("x", edObject.x, edObject.setX),
              _labeledIntegerInput("y", edObject.y, edObject.setY),
              labeledIntegerInput("z", edObject.z),
              labeledIntegerInput("i", edObject.i),
              labeledIntegerInput("addFlags", edObject.addFlags),
              labeledIntegerInput("dynamicFlags", edObject.dynamicFlags),

              labeledCheckboxInput("noDraw", edObject.noDraw),
              labeledCheckboxInput("mirror", edObject.mirror),
              labeledCheckboxInput("invert", edObject.invert),
              labeledCheckboxInput("flash", edObject.flash),

              labeledIntegerInput("userFlags", edObject.userFlags),
              labeledIntegerInput("score", edObject.score),
              labeledIntegerInput("points", edObject.points),
              labeledIntegerInput("powerUp", edObject.powerUp),
              labeledIntegerInput("damage", edObject.damage),
              labeledIntegerInput("smarts", edObject.smarts),
              labeledIntegerInput("health", edObject.health),
              labeledIntegerInput("userValue1", edObject.userValue1),
              labeledIntegerInput("userValue2", edObject.userValue2),
              labeledIntegerInput("userValue3", edObject.userValue3),
              labeledIntegerInput("userValue4", edObject.userValue4),
              labeledIntegerInput("userValue5", edObject.userValue5),
              labeledIntegerInput("userValue6", edObject.userValue6),
              labeledIntegerInput("userValue7", edObject.userValue7),
              labeledIntegerInput("userValue8", edObject.userValue8),
            ).flatten),
            column(List(
              labeledIntegerInput("xMin", edObject.xMin),
              labeledIntegerInput("yMin", edObject.yMin),
              labeledIntegerInput("xMax", edObject.xMax),
              labeledIntegerInput("yMax", edObject.yMax),
              labeledIntegerInput("speedX", edObject.speedX),
              labeledIntegerInput("speedY", edObject.speedY),
              labeledIntegerInput("xTweak", edObject.xTweak),
              labeledIntegerInput("yTweak", edObject.yTweak),
              labeledIntegerInput("counter", edObject.counter),
              labeledIntegerInput("speed", edObject.speed),
              labeledIntegerInput("width", edObject.width),
              labeledIntegerInput("height", edObject.height),
              labeledIntegerInput("direction", edObject.direction),
              labeledIntegerInput("faceDir", edObject.faceDir),
              labeledIntegerInput("timeDelay", edObject.timeDelay),
              labeledIntegerInput("frameDelay", edObject.frameDelay),
              labeledIntegerInput("objectType", edObject.objectType),
              labeledIntegerInput("hitTypeFlags", edObject.hitTypeFlags),
              labeledIntegerInput("xMoveRes", edObject.xMoveRes),
              labeledIntegerInput("yMoveRes", edObject.yMoveRes),
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
