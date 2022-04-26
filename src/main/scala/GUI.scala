import DragAndDropTest.stage
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Rectangle}
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
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import scalafx.stage.{StageStyle, WindowEvent}
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

  var ColumnList = Seq[Column]()
  var CardList = Seq[Card]()

  var dragActive = false
  var xmax = 120
  var lockcolumns = true
  val columntopy = 50
  var tick = 0

  var detectonCircle = new Circle {
        radius = 5
        mouseTransparent = true
        visible = false
  }

  private val borderStyle            = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 5;" +
    "-fx-padding: 6;"

  val panelsPane = new Pane() {
      maxWidth = 800
      maxHeight = 500

      val test = new Column().co

      var newColumnButton = new Button("add column") {
        onAction = _ => {
          val newColumn = new Column().co
          children += newColumn
          newColumn.relocate(this.layoutX.value, this.layoutY.value)
          this.relocate(this.layoutX.value + 120, this.layoutY.value)
          xmax += 120
        }
      }
      val dragModeCheckbox = new CheckBox("lock columns") {
        margin = Insets(6)
      }

      test.relocate(0, columntopy)
      newColumnButton.relocate(120,columntopy)
      dragModeCheckbox.relocate(0,0)
      detectonCircle.relocate(0,0)

      children = Seq(test, newColumnButton, dragModeCheckbox, detectonCircle)
  }

  stage = new JFXApp.PrimaryStage {

    System.currentTimeMillis()

    title = "Kabaan app"
    scene = new Scene(800,600) {

      onMouseDragged = (me: MouseEvent) => {
        //println("current choords: " + me.sceneX + " " + me.sceneY)
        dragActive = true
        //detectonCircle.relocate(detectonCircle.translateX.value-me.x , detectonCircle.translateY.value-me.y)
      }

      onMouseMoved = (me: MouseEvent) => {
        detectonCircle.relocate(me.x-detectonCircle.radius.value/2, me.y-detectonCircle.radius.value/2)
        //println("current choords: " + me.sceneX + " " + me.sceneY)
        //detectonCircle.relocate(detectonCircle.translateX.value - me.x , detectonCircle.translateY.value - me.y)
      }

      //intersection
      root = new BorderPane() {
          top = panelsPane
      }
    }
  }

   private var startX: Double = 0d
   private var startY: Double = 0d
   private var nodeX:  Double = 0d
   private var nodeY:  Double = 0d

  var currentlyHovered: Option[Node] = None
  var currentlyDragged: Option[Node] = None
  var currentData: Option[String] = None
  var overlapped = (None, false)

  def newText(s: String): Node = new TextField() {
          prefWidth = 90
          prefHeight = 10
          promptText = s
  }




  class Column(title: String = "Title", cards: Seq[Card] = Seq()) {
    ColumnList = ColumnList :+ this
    var co: VBox = new VBox(6) {
      prefWidth = 100
      prefHeight = 50
      val header = newText(title)
      val addCardButton = new Button("Add card") {
      onAction = _ => {
        addCard()
      }
      }
      children = Seq(header, addCardButton)
      style = borderStyle
    }

    if(cards.nonEmpty) cards.foreach(this.addCustomCard(_))

    def relocate(x: Double, y: Double) = co.relocate(x,y)
    def addCard(): Unit = {
      val next = new Card(this)
      CardList = CardList :+ next
      co.children.add(next.ca)
    }
    def removeCard(n: Node): Unit = {
      var removed = n
      if(co.children.map(_.getId).contains(n.getId)) {
        co.getChildren.remove(n)
      }
    }
    def addCustomCard(card: Card): Unit = {
      co.children.add(card.ca)
    }
  }




  class Card(parent: Column, data: Node = newText("cardtext")) {
    CardList = CardList :+ this
    var p = parent
    def deleteThis(): Unit = p.removeCard(ca)
    var ca: VBox = new VBox(6) {
      prefWidth = 100
      prefHeight = 50
      val text = data

      children = Seq(text)
      style = borderStyle

      def undoDrag() = {
        println("undo")
        this.translateX = 0
        this.translateY = 0
      }
      //detect interaction
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if(!dragActive)  {
            me.eventType match {
              case MouseEvent.MousePressed =>
                  //this.toFront()
                  startX = me.sceneX
                  startY = me.sceneY
                  nodeX = this.translateX() - me.sceneX
                  nodeY = this.translateY() - me.sceneY
              case MouseEvent.MouseMoved =>
              case _ =>
            }
          }
          else if (dragActive) {
            me.eventType match {
              case MouseDragEvent.MouseDragEntered => {
                currentlyHovered = Some(this.parent.get())
                currentlyDragged = Some(this)
                println(currentlyHovered)
              }
              case MouseDragEvent.MouseDragExited => currentlyHovered = None
              case MouseEvent.MouseDragged => detectonCircle.relocate(me.sceneX-detectonCircle.radius.value/2, me.sceneY-detectonCircle.radius.value/2)
                dragActive = true
                this.translateX = nodeX + me.sceneX
                this.translateY = nodeY + me.sceneY

              case MouseEvent.MouseReleased =>
                if(checkOverlapp().nonEmpty && dragActive){
                  deleteThis()
                  var goalColumn = ColumnList.find(a => a.co.toString.contains(checkOverlapp().head.toString))
                  goalColumn.get.addCustomCard(Card.this)
                  this.undoDrag()
                } else undoDrag()
                currentlyDragged = None
                dragActive = false
                println("released")
              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }

          def checkOverlapp(): Seq[Node] = {
            var i: Seq[Node] = Seq()
            var columns = detectonCircle.parent.get().getChildrenUnmodifiable.filter(_.getClass.toString.contains("VBox"))
            for(n <- columns) { //check intersections
              if(n.boundsInParent.value.intersects(detectonCircle.boundsInParent.value)){
                if(!this.parent.toString.contains(n.toString)) i = i:+n
              }
            }
            i
          }
          def changeColumn() = {
           println("change")

          }
      }
    }

  }

}
/*
private val columnDragLock = new BooleanProperty(this, "lock columns", true)
  var dragActive = false
  var xmax = 120

  private val borderStyle            = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 5;" +
    "-fx-padding: 6;"

  stage = new JFXApp.PrimaryStage {
    title = "Kabaan board"
    scene = new Scene(800,600) {

    val topPane = new HBox() {
      maxWidth = 800
      maxHeight = 50

      var title = new TextField(){
        maxWidth = 140
        maxHeight = 30
      }
      val dragModeCheckbox = new CheckBox("lock columns") {
        margin = Insets(6)
        selected = columnDragLock()
      }
      title.relocate(0,0)
      dragModeCheckbox.relocate(100,0)
      children = Seq(title, dragModeCheckbox)
    }

    val columnPane = new Pane() {
      maxWidth = 800
      maxHeight = 550

      var column1 = makeDraggable(createColumn())
      var newColumnButton = new Button("add column") {
        onAction = _ => {
          val newColumn = makeDraggable(createColumn())
          children += newColumn
          newColumn.relocate(this.layoutX.value, this.layoutY.value)
          this.relocate(this.layoutX.value + 120, this.layoutY.value)
          xmax += 120
        }
      }
      column1.relocate(0,40)
      newColumnButton.relocate(120,40)
      children = Seq(column1, newColumnButton)
    }

      onMouseDragged = (me: MouseEvent) => {
        dragActive = true
      }

      onMouseMoved = (me: MouseEvent) => {
        println("current choords moved: " + me.x + " " + me.y + " and xmax: " + xmax)
      }

      root = new BorderPane() {
        top = topPane
        center = columnPane
      }

      def columnIntersections(node: Node): Unit = {
        var intersection: Seq[Node] = Seq()
        for(n <- columnPane.children) {
        }
      }


    }
  }

  private final class dragValues {
    var startX: Double = 0d
    var startY: Double = 0d
    var nodeX:  Double = 0d
    var nodeY:  Double = 0d
  }

  val columnXPositions = Seq(0, 120)

  private def makeDraggable(node: Node): Node = {

    val dragChoords = new dragValues()
    def undoDrag() = {
      println("undo")
      node.translateX = 0
      node.translateY = 0
    }
    def swapPositions() = {

    }
    new Group(node) {
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if(!dragActive)  {
            me.eventType match {
              case MouseEvent.MousePressed =>
                  dragChoords.startX = node.translateX.value
                  dragChoords.startY = node.translateY.value
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
                  if(me.sceneX >= xmax || me.sceneY < 40) undoDrag()
                  else {
                    println("normal drop")
                    println("screnx: " + me.sceneX)
                    println("localx: " + node.parentToLocal(me.sceneX, 40))
                    node.translateX =node.parentToLocal ((me.sceneX/120 - 1).floor * 120, 0).getX
                    node.translateY = 0
                  }
              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
          else {}
      }
      //support functions
      def undoDrag() = {
          println(xmax)
          println(node.translateX.value)
          println("undo")
          node.relocate(dragChoords.startX, dragChoords.startY)
      }
      def checkOverlapp() = {

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

    children = Seq(title, addCard)
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
 */
