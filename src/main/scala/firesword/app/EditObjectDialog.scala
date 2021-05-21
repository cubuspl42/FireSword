package firesword.app

import firesword.app.EdObject.EdObject
import firesword.app.Editor.Editor
import firesword.dom.Dom.Tag.{button, div, integerInput, span}
import firesword.dom.Dom.{IntegerInput, Widget}
import firesword.frp.MutCell.MutCell
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLInputElement


object EditObjectDialog {

  import firesword.frp.DynamicList.Implicits.implicitStatic
  import firesword.frp.Frp.{implicitConst, implicitConstSome}

  private def labeledIntegerInput(label: String, property: MutCell[Int]): List[Widget] = {
    val input = integerInput(property.sample())

    input.value.listen(property.set)

    List(
      span(s"${label}: "),
      input,
    )

  }


  def editObjectDialog(
                        editor: Editor,
                        edObject: EdObject,
                      ): Widget = {
    import firesword.frp.DynamicList.Implicits.implicitStaticSingleton
    //    val input = document.createElement("input")
    //      .asInstanceOf[HTMLInputElement]


    def column(children: List[Widget]): Widget = {
      div(
        inlineStyle =
          """
            |    display: grid;
            |    grid-template-columns: max(100px) 80px;
            |    grid-gap: 5px;
            |""".stripMargin,
        children = children,
      )
    }

    val saveButton = button("Save")

    saveButton.onPressed.listen(_ => editor.stopEditObject())

    div(
      styleClass = MyStyles.editObjectDialogWrapper,
      children = List(
        div(
          styleClass = MyStyles.editObjectDialog,
          children = List(
            div(
              styleClass = MyStyles.editObjectDialogRow,
              children = List(
                column(List(

                  labeledIntegerInput("id", edObject.id),

                  //            labeledIntegerInput("name", edObject.name),
                  //            labeledIntegerInput("logic", edObject.logic),
                  //            labeledIntegerInput("imageSet", edObject.imageSet),
                  //            labeledIntegerInput("animation", edObject.animation),

                  labeledIntegerInput("x", edObject.x),
                  labeledIntegerInput("y", edObject.y),
                  labeledIntegerInput("z", edObject.z),
                  labeledIntegerInput("i", edObject.i),
                  labeledIntegerInput("addFlags", edObject.addFlags),
                  labeledIntegerInput("dynamicFlags", edObject.dynamicFlags),
                  labeledIntegerInput("drawFlags", edObject.drawFlags),
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
        ),
      ),
    )
  }
}
