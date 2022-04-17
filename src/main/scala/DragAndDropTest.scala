import GUI.{DragContext, borderStyle, dragModeActiveProperty}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.{Group, Node, Scene, control, shape}
import scalafx.scene.image.ImageView
import scalafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseDragEvent, MouseEvent}
import scalafx.scene.control.{Button, CheckBox, Label, TextField}
import scalafx.scene.layout.{BorderPane, HBox, Pane, VBox}
import scalafx.scene.shape.{Circle, Rectangle, Shape}
import scalafx.scene.paint.Color



object DragAndDropTest extends JFXApp{

  var dragActive = false

  private val borderStyle            = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 5;" +
    "-fx-padding: 6;"

  stage = new JFXApp.PrimaryStage {
    title = "Drag and drop test"
    scene = new Scene(800,600) {

    val panelsPane = new Pane() {
      maxWidth = 800
      maxHeight = 600

      val test = makeDraggable(createCard())
      val test2 = makeDraggable(createCard())
      test.relocate(50,50)
      test2.relocate(100,100)

      children = Seq(test, test2)
    }

      onMouseDragged = (me: MouseEvent) => {
        println("current choords: " + me.x + " " + me.y)
        dragActive = true
      }

      onMouseMoved = (me: MouseEvent) => {
        println("current choords: " + me.x + " " + me.y)
      }

      //intersection
       root = new BorderPane() {
          center = panelsPane
        }


    }
  }

  private final class dragValues {
    var stagex: Double = 0d
    var stagey: Double = 0d
    var nodeX:  Double = 0d
    var nodeY:  Double = 0d
  }

  private def makeDraggable(node: Node): Node = {

    val dragChoords = new dragValues()

    new Group(node) {
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if(!dragActive)  {
            me.eventType match {
              case MouseEvent.MousePressed =>
                  dragChoords.nodeX = stage.x.value
                  dragChoords.nodeY = stage.y.value
                  dragChoords.nodeX = node.translateX() - me.x
                  dragChoords.nodeY = node.translateY() - me.y
              case _ =>
            }
          }
          else if (dragActive) {
            me.eventType match {
              case MouseEvent.MouseDragged =>
                println("current choords: " + me.x + " " + me.y)
                dragActive = true
                node.translateX = dragChoords.nodeX + me.x
                node.translateY = dragChoords.nodeY + me.y
              case MouseEvent.MouseReleased =>
                if(dragActive) dragActive = false
              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
      }
    }
  }

  def createCard(): Node = new VBox(6) {
    var currentlyselected = false
    prefWidth = 100
    prefHeight = 150
    val acceptanceLabel = new TextField() {
      promptText = "a field"
    }
    children = Seq(new Label("title"),
      acceptanceLabel
    )
    if(this.hover.value) println("card hovered")
    //alignment = Pos.CenterLeft
    style = borderStyle
  }

 //def checkOverlapp
  //click == press and release
  //press = pressdown mouse
  //moved = move unpressed mouse
  //dragged = move pressed mouse


}
