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


object GUI extends JFXApp {

  //changeable base variables
  var columnBackgroundColor = "white"
  var columnWidth = 150

  //active column and card tracking
  var ColumnList = Seq[Column]()
  var CardList = Seq[Card]()

  //drop down options for cards
  var cardTypes = Seq("field", "area", "img", "slider")
  var typeChanged = false
  var cardColors = Seq("White", "Black", "LightGray", "LightBlue", "LightGreen", "LightYellow", "Red", "LightCyan")
  var colorChanged = false
  //card.data = selected type< (wip for custom card types)

  def borderStyle() = {"" +
    "-fx-background-color: " + columnBackgroundColor +
    ";-fx-border-color: Black" +
    ";-fx-border-width: 1" +
    ";-fx-border-radius: 5" +
    ";-fx-padding: 6;"
  }

  var dragActive = false
  var xmax = columnWidth + 20
  val columntopy = 50

  var detectonCircle = new Circle { //helper for drag and drop function
        radius = 5
        mouseTransparent = true
        visible = false
  }

  //scene main components
  val panelsPane = new Pane() {

      maxWidth = 800
      maxHeight = 500

      val title = new HBox(){
          prefWidth = 150
          prefHeight = 30
          val label = new Label("title")
          var input = new TextField(){
              visible = true
              prefWidth = 100
              prefHeight = 20
              promptText = label.text.value
          }
          label.font = new Font("Cabria", 15)
          onMouseEntered = (me: MouseEvent) => {
              children.add(input)
          }
          onMouseExited = (me: MouseEvent) => {
              children.remove(input)
          }
          input.text.onChange {
            label.text = input.text.value
          }

          children = Seq(label)
          //style = borderStyle
      }
      val cardTypeLabel = new Label("Card type")
      val cardTypeSelector = new ComboBox(cardTypes)
      cardTypeSelector.onAction = (e: Any) => typeChanged = true

      val cardColorLabel = new Label("Card color")
      val cardColorSelector = new ComboBox(cardColors)
      cardColorSelector.onAction = (e: Any) => println(cardTypeSelector)

      val column0 = new Column().co
      var newColumnButton = new Button("add column") {
          onAction = _ => {
              val newColumn = new Column().co
              children += newColumn
              newColumn.relocate(this.layoutX.value, this.layoutY.value)
              this.relocate(this.layoutX.value + columnWidth + 20, this.layoutY.value)
              xmax += columnWidth + 20
          }
      }

      title.relocate(5,10)
      cardTypeLabel.relocate(150,0)
      cardTypeSelector.relocate(150, 15)
      cardColorLabel.relocate(250,0)
      cardColorSelector.relocate(250,15)
      column0.relocate(0, columntopy)
      newColumnButton.relocate(columnWidth+20,columntopy)
      detectonCircle.relocate(0,0)

      children = Seq(title, cardTypeLabel, cardTypeSelector, cardColorLabel, cardColorSelector, column0, newColumnButton, detectonCircle)
  }

  stage = new JFXApp.PrimaryStage {

      System.currentTimeMillis()

      title = "Kabaan app"
      scene = new Scene(800,600) {

          onMouseDragged = (me: MouseEvent) => {
              //println("current choords: " + me.sceneX + " " + me.sceneY)
              dragActive = true
          }
          onMouseMoved = (me: MouseEvent) => {
             detectonCircle.relocate(me.x-detectonCircle.radius.value/2, me.y-detectonCircle.radius.value/2)
          }

          root = new BorderPane() {
              top = panelsPane
          }
      }
  }

  //cards and column classes

  class Column(title: String = "Title", cards: Seq[Card] = Seq()) {

      ColumnList = ColumnList :+ this

      var co: VBox = new VBox(6) {  // card node
          prefWidth = columnWidth
          prefHeight = 50
          val header = newTextField(title)
          val addCardButton = new Button("Add card") {
              onAction = _ => {
                addCard()
              }
          }

          children = Seq(header, addCardButton)
          style = borderStyle()
      }

      if(cards.nonEmpty) cards.foreach(this.addCustomCard(_)) //helper for creating board

      //column functions
      def relocate(x: Double, y: Double) = co.relocate(x,y)
      def addCard(): Unit = {
          val next = {
            if(typeChanged) new Card(this, cardType())
            else new Card(this)
          }
          CardList = CardList :+ next
          co.children.add(next.ca)
      }
      def addCustomCard(card: Card): Unit = {
          co.children.add(card.ca)
          card.p = this
      }
      def removeCard(n: Node): Unit = {
          var removed = n
          if(co.children.map(_.getId).contains(n.getId)) {
            co.getChildren.remove(n)
          }
      }

  }

  //drag and drop helpers
   private var nodeX:  Double = 0d
   private var nodeY:  Double = 0d

   def cardType(): Node = panelsPane.cardTypeSelector.value.value match {
     case "field" => newTextField("textfield")
     case "area" => newTextarea("textarea")
     //case "img" =>
     //case "slider" =>
   }

   def  cardColor() = println("yes")





  class Card(parent: Column, data: Node = newTextField("cardtext")) {

      CardList = CardList :+ this

      var p: Column = parent

      var ca: VBox = new VBox(6) {
          prefWidth = columnWidth
          prefHeight = 50
          var d = data

          children = Seq(d)
          style = borderStyle()

      //detect interaction for cards
      filterEvent(MouseEvent.Any) {
          me: MouseEvent =>
              if(!dragActive)  {
                  me.eventType match {
                      case MouseEvent.MousePressed =>
                          nodeX = this.translateX() - me.sceneX
                          nodeY = this.translateY() - me.sceneY
                      case _ =>
                  }
              }
              else if (dragActive) {
                  me.eventType match {
                      case MouseEvent.MouseDragged => detectonCircle.relocate(me.sceneX-detectonCircle.radius.value/2, me.sceneY-detectonCircle.radius.value/2)
                          dragActive = true
                          this.toFront()
                          this.translateX = nodeX + me.sceneX
                          this.translateY = nodeY + me.sceneY
                          this.toFront()
                      case MouseEvent.MouseReleased =>
                          if(checkOverlapp().nonEmpty && dragActive){
                              deleteThis()
                              var goalColumn = ColumnList.find(a => a.co.toString.contains(checkOverlapp().head.toString))
                              goalColumn.get.addCustomCard(Card.this)
                              this.undoDrag()
                          }
                          else undoDrag()
                          dragActive = false
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
      }

          //node functions
          def undoDrag() = {
              this.translateX = 0
              this.translateY = 0
          }

     }

      //card functions

      def deleteThis(): Unit = p.removeCard(ca)

  }

  def newTextField(s: String): Node = new TextField() {
          println("field")
          prefWidth = columnWidth - 10
          prefHeight = 10
          promptText = s
  }

  def newTextarea(s: String): Node = new TextArea() {
          println("area")
          prefWidth = columnWidth - 10
          prefHeight = 20
          promptText = s
  }

}
