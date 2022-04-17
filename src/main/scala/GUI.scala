import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Font
import scalafx.scene.control.Label
import scalafx.scene.control.CheckBox
import scalafx.scene.control.Button
import scalafx.scene.layout.StackPane
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.Background
import scalafx.scene.layout.BackgroundFill
import scalafx.scene.layout.CornerRadii
import scalafx.scene.layout.ColumnConstraints
import scalafx.scene.layout.RowConstraints
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.stage.{WindowEvent, StageStyle}
import scalafx.beans.property.BooleanProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.{Group, Node, Scene}

import api.Card
import api.Column
import api.Board



object GUI extends JFXApp {

  var dragActive = false
  var columnMaxX = 240

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

      var column1 = makeDraggable(createColumn())
      var column2 = createColumn()
      var newColumnButton = new Button("add column") {
        onAction = _ => {
        val newColumn = makeDraggable(createColumn())
        children += newColumn
        newColumn.relocate(this.layoutX.value, this.layoutY.value)
          this.relocate(this.layoutX.value + 120, this.layoutY.value)
        }
      }
      column1.relocate(0,0)
      //column2.relocate(120,0)
      newColumnButton.relocate(120,0)

      children = Seq(column1, newColumnButton)
    }

      onMouseDragged = (me: MouseEvent) => {
        println("current choords dragged: " + me.x + " " + me.y)
        dragActive = true
      }

      onMouseMoved = (me: MouseEvent) => {
        println("current choords moved: " + me.x + " " + me.y)
      }

      root = new BorderPane() {
        center = panelsPane
      }

      def columnIntersections(node: Node): Unit = {
        var intersection: Seq[Node] = Seq()
        for(n <- panelsPane.children) {
        }
      }


    }
  }

  private final class dragValues {
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
                println("current choords node dragged: " + me.sceneX + " " + me.sceneY)
                dragActive = true
                node.translateX = dragChoords.nodeX + me.x
                node.translateY = dragChoords.nodeY + me.y
              case MouseEvent.MouseReleased =>
                dragActive = false

              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
      }
    }
  }



  def createColumn(): Node = new VBox(6) {
    prefWidth = 100
    prefHeight = 50
    val isColumn = true

    val title = new TextField() {
      promptText = "title"
    }

    val addCard = new Button("Add card") {
      onAction = _ => {
        val next = makeDraggable(createCard())
        children += next
        println(children)
      }
    }

    children = Seq(title, addCard
    )
    style = borderStyle

    def createCard(): Node = new VBox(6) {
      val isColumn = false
      prefWidth = 100
      prefHeight = 10
      val text = new TextField() {
        promptText = "card text"
      }
      children = Seq(text)
      //alignment = Pos.CenterLeft
      style = borderStyle
    }
  }

}
