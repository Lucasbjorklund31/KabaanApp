import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.control._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Font
import java.util.Calendar

object GUI extends JFXApp {

  //Editgables

  val windowWidth = 800
  val windowHeigth = 600
  //background variables
   var backgroundColor = "white"

  //column variables
  var columnBackgroundColor = "white"
  var columnWidth = 150


  //Stock
  //background variables
  var boardList = Seq[Board]()

  var currentTitle = ""
  var currentBoard: Option[Board] = None
  var currentMenuBoard: Option[boardDisplayBox] = None
  var columnList = Seq[Column]()
  var cardList = Seq[Card]()
  var cardArcive = Seq[Card]()

  var dragActive = false
  var isnewboard = false
  var xmax = columnWidth + 20
  val columntopy = 50




  //drop down options for cards
  var cardTypes = Seq("field", "area", "checkbox", "slider")
  var typeChanged = false
  var cardColors = Seq("White", "Black", "LightGray", "LightBlue", "LightGreen", "LightYellow", "Red", "Cyan", "Pink", "Purple", "Brown", "Violet")
  var colorChanged = false

  val border = { //stock borders
    ";-fx-border-color: Black" +
    ";-fx-border-width: 1" +
    ";-fx-border-radius: 5" +
    ";-fx-padding: 6;"
  }

  def columnStyle() = {
    "-fx-background-color: " + columnBackgroundColor +
    border
  }
  def cardStyle() = {
    "-fx-background-color: " + cardColorSelector.value.value +
    border
  }

  //board objects that can be accessed globally
  var detectonCircle = new Circle {                                             //helper for drag and drop functions
        radius = 5
        mouseTransparent = true
        visible = false
  }
  val cardTypeSelector = new ComboBox(cardTypes)                                //changes the type of new cards
      cardTypeSelector.onAction = (e: Any) => typeChanged = true

  val cardColorSelector = new ComboBox(cardColors)                              //changes the color of new cards
      cardColorSelector.onAction = (e: Any) => println(cardTypeSelector)

  val removeBox = new VBox {                                                    //box which adds cards to the archive or completely removes them ón drag drop
          visible = false
          prefWidth = 100
          prefHeight = 45
          val title = new Label("Delete Card")

          children = Seq(title)
          style = columnStyle()
  }
  val archive = new Column() {                                                    //using a column as base to make drag and drop functions work
          co.children.clear()                                                     //displays archived cards upon hower
          co.prefWidth = 80
          co.prefHeight = 20

          val title = new Label("Archive")
          co.onMouseEntered = (me: MouseEvent) => {
              cardArcive.foreach(a => this.addCustomCard(a))
          }

          co.children = Seq(title)
          co.style = columnStyle()

          def hideCards() = { //hides the cards the archive while the archive is not interacted with
              co.children.clear()
              co.children.add(title)
          }
  }




  class Board extends JFXApp {
        var title: String = ""
        var lastEdited: String = ""
        var columns: Seq[Column] = Seq[Column]()
        var cards: Seq[Card] = Seq[Card]()
        var menuBox = None
  }

  def createBoard(): Board = {
            val next = new Board()
            isnewboard = true
            currentBoard = Some(next)
            boardList = boardList :+ next
            stage = mainBoard()
            next
  }

  def updateBoardMenu() = {
    val nextBox = new boardDisplayBox(boardList.last)
    currentMenuBoard = Some(nextBox)
    loadBoardMenu.children.add( nextBox.box )
    loadBoardMenu.children.filter(_.toString.contains(""))
    isnewboard = false
  }



   class boardDisplayBox(b: Board) {
        val relatedBoard = b
        def updateTitle(s: String) = box.title.text = s
        val box = new HBox {
            val title = new Label("Untitled"){
              visible = true
              font = new Font("Cabria", 10)
            }
            var edit = new Button("Edit board") {
                onAction = _ => {
                  currentBoard = Some(b)
                  columnList = b.columns
                  cardList = b.cards
                  currentTitle = b.title
                  currentMenuBoard = Some(boardDisplayBox.this)
                  showBoard()
                }
            }

            children = Seq(edit, title)
            style = GUI.border
        }
   }

   def resetBoard(): Unit = {
      var edited = currentBoard.get
      edited.title = currentTitle
      edited.columns = Seq()
      if(columnList.nonEmpty) for(column <- columnList) edited.columns = edited.columns :+ column
      columnList = Seq()
      edited.cards = Seq()
      if(cardList.nonEmpty) for(card <- cardList) edited.cards = edited.cards :+ card
      cardList = Seq()
      currentBoard = None
      currentTitle = ""
  }

  //scene main/starter components
  def panelsPane(loadColumn: Seq[Column] = Seq(), loadCards: Seq[Card] = Seq(), loadTitle: String = ""): Pane = new Pane() {

      maxWidth = windowWidth
      maxHeight = windowHeigth

      //top panel
      val title = new HBox() {
          prefWidth = 150
          prefHeight = 30
          val label = new Label("Title")
          var input = new TextField(){
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
             currentTitle = input.text.value
             label.text = input.text.value
          }

          children = Seq(label)
      }

      val cardTypeLabel = new Label("Card type")

      val cardColorLabel = new Label("Card color")

      val taggFilterLabel = new Label("Tagg filter")
      val taggFilter = new TextField(){
          prefWidth = 100
          prefHeight = 20
          promptText = "Card tagg filter"
      }
      taggFilter.text.onChange {
          cardList.foreach(a =>
          if(taggFilter.text.value.isEmpty) a.ca.visible = true
          else {
          if(a.cardTagg == taggFilter.text.value) a.ca.visible = true
          else a.ca.visible = false })
      }

      //columns and card functions
      val column0 = new Column()
      var newColumnButton = new Button("Add column") {
          onAction = _ => {
              val newColumn = new Column()
              children += newColumn.co
              newColumn.relocate(this.layoutX.value, this.layoutY.value)
              this.relocate(this.layoutX.value + columnWidth + 20, this.layoutY.value)
              xmax += columnWidth + 20
          }
      }

      val saveBoard = new Button("Save board") {
          onAction = _ => {
              println(Calendar.getInstance().getTime.toString.dropRight(9) + Calendar.getInstance().getTime.toString.takeRight(4))
              var title = (currentTitle + " - Last edited: "+ Calendar.getInstance().getTime.toString.dropRight(9) + Calendar.getInstance().getTime.toString.takeRight(4))
              columnList = columnList.filter(_.co != archive.co)      //removing the archive column from the columnlist to avoid dublicates
              if(currentTitle.nonEmpty) currentMenuBoard.get.updateTitle(title)
              showMenu()
          }
      }

      onMouseMoved = (me: MouseEvent) => {
        if(dragActive || !archive.co.hover.value) {
        archive.hideCards()
        archive.co.toFront()
        }
        if(isnewboard) updateBoardMenu()
      }

      //node positions
      title.relocate(5,10)
      cardTypeLabel.relocate(150,0)
      cardTypeSelector.relocate(150, 20)
      cardColorLabel.relocate(250,0)
      cardColorSelector.relocate(250,20)
      taggFilterLabel.relocate(380,0)
      taggFilter.relocate(380,20)
      removeBox.relocate(380, 0)
      archive.relocate(500, 15)
      saveBoard.relocate(600, 10)

      column0.co.relocate(0, columntopy)
      newColumnButton.relocate(columnWidth+20,columntopy)
      detectonCircle.relocate(0,0)

      children = Seq(archive.co, title, cardTypeLabel, cardTypeSelector, cardColorLabel, cardColorSelector, taggFilterLabel, taggFilter, newColumnButton, removeBox, saveBoard, detectonCircle)

      if(loadColumn.nonEmpty) loadColumn.foreach(c => children.add(c.co))
      if(loadCards.nonEmpty) loadCards.foreach(c => c.p.addCustomCard(c))
      if(loadTitle != "") title.label.text = loadTitle
      if(isnewboard) children.add(column0.co)    //checking if we have a new board so we know if we should add the stock column
      else {
        columnList = columnList.filter(_ != column0)   // if not remove it from the column list to avoid it being double saved
        newColumnButton.relocate(newColumnButton.layoutX.value + (columnList.length - 1) * (columnWidth + 20), newColumnButton.layoutY.value)  //moving along the column button to match the column amount
      }

  }

  def mainBoard() = {

        new JFXApp.PrimaryStage {

            title = "Kabaan app"
            scene = new Scene(windowWidth,windowHeigth) {

                onMouseDragged = (me: MouseEvent) => {
                    dragActive = true
                }
                onMouseMoved = (me: MouseEvent) => {
                   detectonCircle.relocate(me.x-detectonCircle.radius.value/2, me.y-detectonCircle.radius.value/2)
                }

                root = new BorderPane() {
                    top = {
                      if(isnewboard) {
                        println("is new")
                        panelsPane()
                      }
                      else {
                        println("edit")
                        panelsPane(columnList, cardList, currentTitle)
                      }
                    }
                }

            }
       }
  }

  val loadBoardMenu = new VBox {
              prefWidth = 400
              prefHeight = 100

              val title = new Label("Boards")

              val createBoardButton = new Button("create board"){
                  onAction = _ => {
                      createBoard()
                  }
              }
              style = GUI.border
              children = Seq(title, createBoardButton)
  }

  def boardSelection() = new JFXApp.PrimaryStage {

      title = "Board menu"
      scene = new Scene(windowWidth, windowHeigth) {

          root = new BorderPane() {
              top = new Pane() {

                  onMouseMoved = (me: MouseEvent) => if(currentBoard.isDefined) resetBoard()

                  loadBoardMenu.relocate(0, 0)
                  children = loadBoardMenu
              }

          }

      }
  }


    stage = boardSelection()
    def showBoard() = stage = mainBoard()
    def showMenu() = stage = boardSelection()




    //drag and drop helpers
     private var nodeX:  Double = 0d
     private var nodeY:  Double = 0d

    //card options
     def cardType(): Node = cardTypeSelector.value.value match {
       case "field" => newTextField("textfield")
       case "area" => newTextarea()
       case "slider" => newSlider()
       case "checkbox" => newCheckBox()
     }

    def  cardColor() = cardColorSelector.value.value


    //card types
    def newTextField(s: String): Node = new TextField() {
            prefWidth = columnWidth - 10
            prefHeight = 10
            promptText = s
    }

    def newTextarea(): Node = new TextArea() {
            prefWidth = columnWidth - 10
            prefHeight = 20
            promptText = "textarea"
    }

    def newSlider(): Node = new Slider() {
            prefWidth = columnWidth - 10
            prefHeight = 20
    }

    def newCheckBox() = {

        new VBox {
            var tasks = Seq[CheckBox]()

            def addTask(s: String) = {
                val t = new CheckBox(s)
                children.add(t)
                tasks = tasks :+ t
            }

            var cardTitle = new TextField() {
                promptText = "main task"
            }

            val taskName = new TextField() {
               promptText = "sub task"
            }
            val addTaskButton = new Button("Add task") {
                onAction = _ => {
                    addTask(taskName.text.value)
                    taskName.text = ""
                }
            }
            children = Seq(cardTitle, taskName, addTaskButton)

        }
    }



  //cards and column classes
  class Column(title: String = "Title", cards: Seq[Card] = Seq()) {

      if(columnList == null) columnList = Seq()
      columnList = columnList :+ this
      //if(currentBoard != null) if(currentBoard.isDefined) currentBoard.get.columns = currentBoard.get.columns :+ this

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
          style = columnStyle()
      }

      if(cards.nonEmpty) cards.foreach(this.addCustomCard(_)) //helper for creating board

      //column functions
      def relocate(x: Double, y: Double) = co.relocate(x,y)
      def addCard(): Unit = {
          val next = {
            if(typeChanged) new Card(this, cardType())
            else new Card(this)
          }
          if(cardList == null) cardList = Seq()
          cardList = cardList :+ next
          co.children.add(next.ca)
      }
      def addCustomCard(card: Card): Unit = {
          if(!co.children.contains(card.ca)) co.children.add(card.ca)
          card.p = this
      }
      def removeCard(n: Node): Unit = {
          var removed = n
          if(co.children.map(_.getId).contains(n.getId)) {
            co.getChildren.remove(n)
          }
      }

  }


  class Card(parent: Column, data: Node = newTextField("cardtext")) {

      //CardList = CardList :+ this

      var p: Column = parent
      var cardTagg = ""

      var ca: VBox = new VBox(6) {
          //background = cardStyle()
          prefWidth = columnWidth
          prefHeight = 50
          var d = data
          val tagg = new Label("")
          val input = new TextField(){
              visible = true
              prefWidth = 100
              prefHeight = 20
              promptText = "tagg"
          }
          tagg.font = new Font("Cabria", 10)
          onMouseEntered = (me: MouseEvent) => {
              if(!children.contains(input)) children.add(input)
          }
          onMouseExited = (me: MouseEvent) => {
              children.remove(input)
          }
          input.text.onChange {
              tagg.text = input.text.value
              cardTagg = input.text.value
          }

          children = Seq(tagg, d)
          style = cardStyle()

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
                      case MouseEvent.MouseDragged =>
                          this.parent.get().toFront()
                          detectonCircle.relocate(me.sceneX-detectonCircle.radius.value/2, me.sceneY-detectonCircle.radius.value/2)
                          dragActive = true
                          this.translateX = nodeX + me.sceneX
                          this.translateY = nodeY + me.sceneY
                          removeBox.visible = true

                      case MouseEvent.MouseReleased =>
                          if(checkOverlapp().nonEmpty && dragActive){
                              var goalColumn = columnList.find(a => a.co.toString.contains(checkOverlapp().head.toString))
                              deleteThis()
                              archive.hideCards()
                              if(cardArcive.contains(Card.this) && goalColumn.isEmpty) cardArcive = cardArcive.filter(_ != Card.this) //if card already is in the archive and is removed it will be removed from the board completely
                              else {
                                  cardArcive = cardArcive.filter(_ != Card.this)                                                      //removes card from archive so it can be added to its new parent
                                  if(goalColumn.isEmpty) cardArcive = cardArcive :+ Card.this                                         //if the parent is empty aka the remove box => add to archive
                                  else goalColumn.get.addCustomCard(Card.this)                                                        //else add to the new column
                              }
                              this.undoDrag()
                          }
                          else undoDrag()
                          dragActive = false
                          removeBox.visible = false
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
          def undoDrag() = { //returns node back to its columnm position before drag
              this.translateX = 0
              this.translateY = 0
          }
     }
      //card functions
      def deleteThis(): Unit = p.removeCard(ca)   //removes the card from its parent to make it easier to move to new parents
  }

}
