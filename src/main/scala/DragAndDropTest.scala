
//import GUI.{borderStyle, makeDraggable}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.control.{Button, CheckBox, TextField}
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import scalafx.scene.layout.{BorderPane, Pane, VBox}
import scalafx.scene.shape.Circle
import scalafx.scene.{Node, Scene}


object DragAndDropTest extends JFXApp{

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

    title = "Drag and drop test"
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
  def createColumn(): Node = new VBox(6) {
    prefWidth = 100
    prefHeight = 50
    val isColumn = true
    val title = new TextField() {
      promptText = "title"
    }
    val addCard = new Button("Add card") {
      onAction = _ => {
        val next = createCard()
        children += next
        println(children)
      }
    }
    def moveCard(n: Node): Unit = {
      this.children += n
    }
    def isDropped(node: Node, droppedID: String): Unit = {
      if(droppedID == this.toString) this.children += node
    }

    hover.value
    /*
    if(dragActive) onDragDropped = (me: MouseEvent) => {
      this.children.add(currentlyDragged.get)
    }
     */

    children = Seq(title, addCard)
    style = borderStyle

    def createCard(): Node = new VBox(6) {
      prefWidth = 100
      prefHeight = 10
      val text = new TextField() {
        prefWidth = 90
        prefHeight = 10
        promptText = "card text"
      }
      this.text.promptText.get()
      children = Seq(text)
      style = borderStyle
      //cardDragInteractions
      //holders
        var startX: Double = 0d
        var startY: Double = 0d
        var nodeX:  Double = 0d
        var nodeY:  Double = 0d

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
                toFront()
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
                //println(checkOverlapp())
                //if(checkOverlapp().isDefined) println(checkOverlapp().get)
                //println(currentlyHovered + "and" + currentlyDragged)
                //println(checkOverlapp())
                dragActive = true
                this.translateX = nodeX + me.sceneX
                this.translateY = nodeY + me.sceneY

              case MouseEvent.MouseReleased =>
                println(this)
                if(checkOverlapp().isDefined){
                  println(checkOverlapp().get.getClass)
                  removeCard(this)
                } else undoDrag()
                currentlyDragged = None
                dragActive = false
                println("released")
                //val moved = removeCard(this)
                //println(moved._1 + " and " + moved._2)

              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
          /*
          val vboxproof = packet.children.get(0) match {
              case v : javafx.scene.layout.VBox => v.children.get(0).userData
              case v => "Unexpected type: " + v.getClass.getName
          }
           */
          def checkOverlapp(): Option[Node] = {
            var i: Seq[Node] = Seq()
            var columns = detectonCircle.parent.get().getChildrenUnmodifiable.filter(_.getClass.toString.contains("VBox"))
            for(n <- columns) { //check intersections
              if(n.boundsInParent.value.intersects(detectonCircle.boundsInParent.value)) i = i:+n
            }
            //intersections.filter(!_.toString.contains(this.parent.value.toString))
            if(i.exists(!_.toString.contains(this.parent.value.toString))) Some(i.filter(!_.toString.contains(this.parent.value.toString)).head)
            else None
          }
          def changeColumn() = {
           println("change")

          }
      }
    }
    def columnPrint() = {
      println(this)
    }

    def removeCard(n: Node): (Node, Boolean) = {
      var removed = n
      if(this.children.map(_.getId).contains(n.getId)) {
        this.getChildren.remove(n)
      (removed, true)
      } else (removed, false)
    }
    def addCustomCard(what: Node, where: Node) = {
      where
    }
  }
   */
  /*
  def createColumn(): Node = new VBox(6) {
    prefWidth = 100
    prefHeight = 50
    val isColumn = true
    val title = new TextField() {
      promptText = "title"
    }
    val addCard = new Button("Add card") {
      onAction = _ => {
        val next = createCard()
        children += next
        println(children)
      }
    }

    children = Seq(title, addCard)
    style = borderStyle


    def createCard(): Node = new VBox(6) {
      prefWidth = 100
      prefHeight = 10
      val text = new TextField() {
        prefWidth = 90
        prefHeight = 10
        promptText = "card text"
      }
      children = Seq(text)
      style = borderStyle
      //cardDragInteractions
      //holders
        var startX: Double = 0d
        var startY: Double = 0d
        var nodeX:  Double = 0d
        var nodeY:  Double = 0d
      //detect interaction
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if(!dragActive)  {
            me.eventType match {
              case MouseEvent.MousePressed =>
                  startX = me.sceneX
                  startY = me.sceneY
                  nodeX = this.translateX() - me.x
                  nodeY = this.translateY() - me.y
              case _ =>
            }
          }
          else if (dragActive) {
            me.eventType match {
              case MouseEvent.MouseDragged =>
                println("current choords node dragg: " + me.sceneX + " " + me.sceneY)
                dragActive = true
                this.translateX = nodeX + me.x
                this.translateY = nodeY + me.y
              case MouseEvent.MouseReleased =>
                if(dragActive) dragActive = false
                if(me.sceneX >= 240) this.relocate(startX, startY)
                else changeColumn()
              case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
          def checkOverlapp() = {
            var intersections = Seq[Node]()
            for(n <- this.scene.get.content) { //check intersections
              if(n.intersects(this.boundsInLocal.get())) intersections  = intersections :+ n
            }
            for(n <-intersections) { // check most accurate match
              me.sceneX
            }
            intersections
          }
          def changeColumn() = {
           println("change")

          }
      }
    }
  }
   */
/*
    def createColumn(): Node = new VBox(6) {
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
*/
 //def checkOverlapp
  //click == press and release
  //press = pressdown mouse
  //moved = move unpressed mouse
  //dragged = move pressed mouse

  /*
    private def makeDraggable(node: Node): Node = {
    val dragChoords = new dragValues()
    def undoDrag() = {
      println("undo")
      node.translateX = 0
      node.translateY = 0
    }

    new Group(node) {
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if(!dragActive)  {
            me.eventType match {
              case MouseEvent.MousePressed =>
                  dragChoords.startx = me.sceneX
                  dragChoords.starty = me.sceneY
                  dragChoords.nodeX = node.translateX() - me.x
                  dragChoords.nodeY = node.translateY() - me.y
              case _ =>
            }
          }
          else if (dragActive && !lockcolumns) {
            me.eventType match {
              case MouseEvent.MouseDragged =>
                println("current choords node dragg: " + me.sceneX + " " + me.sceneY)
                dragActive = true
                node.translateX = dragChoords.nodeX + me.x
                node.translateY = dragChoords.nodeY + me.y
              case MouseEvent.MouseReleased =>
                if(dragActive) dragActive = false
                if(me.sceneX >= 240) undoDrag()
                else swapPosition()

              //case MouseEvent.MouseMoved =>
              case _ =>
            }
            me.consume()
          }
          def swapPosition() = {
           println("swap")
            // println(scene.value.content(0) )
           println( (me.sceneX/120).floor * 120)
            //translateX = node.sceneToLocal(me.sceneX, 0).getX
          }
      }
    }
  }

    private var startTimeMillis = System.currentTimeMillis()
  private var endTimeMillis = System.currentTimeMillis()

  class Clocker(initial :Long, increment :Long, interval :Long) {
  private val start = LocalTime.now()
  def get :Long =
    initial + SECONDS.between(start, LocalTime.now()) / interval * increment
  }
  val clkr = new Clocker(0, 1, 1)
   */


