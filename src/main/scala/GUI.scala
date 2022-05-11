import javafx.beans.property.ObjectProperty
import javafx.event.EventHandler
import javafx.scene.input
import javafx.scene.input.KeyEvent
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.control._
import scalafx.scene.control.{Button, CheckBox, ProgressBar, TextField}
import scalafx.scene.input.KeyCode.Enter
import scalafx.scene.input.{KeyCode, MouseEvent}
import scalafx.scene.layout._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Font

import java.util.Calendar

object GUI extends JFXApp {

  //Editgables
  val windowWidth =  1000  //800
  val windowHeigth = 800  //600

  val columnWidth = 150   //150
  val columnBackgroundColor = "white"
  //examplecolors: "White", "Black", "LightGray", "LightBlue", "LightGreen", "LightYellow", "Red", "Cyan", "Pink", "Purple", "Brown", "Violet"




  //Stock
  //background variables
  val columntopy = 50
  var boardList = Seq[Board]()
  var currentBoard: Option[Board] = None
  var currentPane: Option[Pane] = None
  var currentTitle = ""
  var columnList = Seq[Column]()
  var cardList = Seq[Card]()
  var cardArcive = Seq[Card]()

  var dragActive = false                        //checks if we currently are mid drag
  var isnewboard = false                        //logs if we have a new board of some kind which still hasn't been saved
  var ispreset1  = false                         //check if we loaded a preset



  //drop down options for cards
  val cardTypes = Seq("Field", "Slider", "Checkbox") //"Area"
  val cardColors = Seq("White", "Black", "LightGray", "LightBlue", "LightGreen", "LightYellow", "Red", "Cyan", "Pink", "Purple", "Brown", "Violet")

  //column and card visuals
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
  val detectonCircle = new Circle {                                             //helper for drag and drop functions
        radius = 5
        mouseTransparent = true
        visible = false
  }
  val cardTypeSelector = new ComboBox(cardTypes) {                              //changes the type of new cards
     value = "Field"
  }


  val cardColorSelector = new ComboBox(cardColors) {                             //changes the color of new cards
    value = "White"
  }

  val removeBox = new VBox {                                                    //box which adds cards to the archive or completely removes them ón drag drop
          visible = false
          prefWidth = 100
          prefHeight = 45
          val title = new Label("Archive Card")

          children = Seq(title)
          style = columnStyle()
  }
  val deleteBox = new VBox {                                                      //box which completely removes them ón drag drop from the archive
          visible = false
          prefWidth = 100
          prefHeight = 45
          val title = new Label("Delete Card")

          children = Seq(title)
          style = columnStyle()
  }
  def hideBoxes() = {
    removeBox.visible = false
    deleteBox.visible = false
  }
  def showBoxes() = {
    removeBox.visible = true
    removeBox.toFront()
    deleteBox.visible = true
    deleteBox.toFront()
  }
  val archive = new Column() {                                                   //using a column as base to make drag and drop functions work
          co.prefWidth = 80
          co.prefHeight = 20

          val title = new Label("Archive")
          co.onMouseEntered = (me: MouseEvent) => {                              //displays archived cards upon hower
              cardArcive.foreach(a => this.addCustomCard(a))
          }

          co.children = Seq(title)
          co.style = columnStyle()

          def hideCards() = {                                                     //hides the cards the archive while the archive is not interacted with
              co.children.clear()
              co.children.add(title)
          }
  }





    //board class
  class Board {
        var title: String = ""
        var columns: Seq[Column] = Seq[Column]()
        var cards: Seq[Card] = Seq[Card]()
        var archivedCards: Seq[Card] = Seq[Card]()
        //val menuBox = new boardDisplayBox(this)
        val menuBox = new HBox {
            val title = new Label("Untitled"){
              visible = true
              font = new Font("Cabria", 11)
            }
            var edit = new Button("Edit") {
                onAction = _ => {
                  currentBoard = Some(Board.this)
                  columnList = Board.this.columns
                  cardList = Board.this.cards
                  currentTitle = Board.this.title
                  showBoard()
                }
            }
            var remove = new Button("Remove") {
                onAction = _ => {
                  deleteThis()
                }
            }

            children = Seq(edit, remove, title)
            style = GUI.border

            def updateTitle(s: String) = this.title.text = s
            def deleteThis() = {
              boardList = boardList.filter(_ == Board.this)
              loadBoardMenu.children.remove(this)
            }
        }
  }

  //board functions
  def createBoard(): Board = {
            val next = new Board()
            currentBoard = Some(next)
            boardList = boardList :+ next
            if(ispreset1) {
                currentBoard.get.title = preset1Title
                currentBoard.get.columns = preset1columns()
            }
            isnewboard = true
            stage = mainBoard()
            next
  }

  def updateBoardMenu() = {
    val nextBox = currentBoard.get.menuBox
    loadBoardMenu.children.add( nextBox )
    loadBoardMenu.children.filter(_.toString.contains(""))
    isnewboard = false
  }

   def resetBoard(): Unit = {
      var edited = currentBoard.get
      edited.title = currentTitle
      edited.columns = Seq()                                                                             //reseting in case of null
      if(columnList.nonEmpty) for(column <- columnList) edited.columns = edited.columns :+ column        //adding all new objects
      columnList = Seq()                                                                                 //clearing active tracker
      edited.cards = Seq()
      if(cardList.nonEmpty) for(card <- cardList) edited.cards = edited.cards :+ card
      cardList = Seq()
      edited.archivedCards = Seq()
      if(cardArcive.nonEmpty) for(card <- cardArcive) edited.archivedCards = edited.archivedCards :+ card
      cardArcive = Seq()
      currentBoard = None
      currentTitle = ""
      cardTypeSelector.value = "Field"
      cardColorSelector.value = "White"
  }


  //scene main/starter components
  def panelsPane(loadColumn: Seq[Column] = Seq(), loadCards: Seq[Card] = Seq(), loadTitle: String = ""): Pane = new Pane() {

      maxWidth = windowWidth
      maxHeight = windowHeigth

      currentPane = Some(this)

      val title = new HBox() {
          prefWidth = 250
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

      val tagFilterLabel = new Label("Tag filter")
      val tagFilter = new TextField(){
          prefWidth = 100
          prefHeight = 20
          promptText = "Card tag filter"
      }
      tagFilter.text.onChange {
          cardList.foreach(a =>
          if(tagFilter.text.value.isEmpty) a.ca.visible = true            //if no filter is entered all are shown
          else {
          if(a.cardTag == tagFilter.text.value) a.ca.visible = true       //else only cards with a matching tag will show
          else a.ca.visible = false })
      }

      //columns and card functions
      val column0 = new Column()
      val newColumnButton = new Button("Add column") {
          onAction = _ => {
              val newColumn = new Column()
              children += newColumn.co
              newColumn.relocate(this.layoutX.value, this.layoutY.value)
              this.relocate(this.layoutX.value + columnWidth + 20, this.layoutY.value)
              removeColumnButton.relocate(removeColumnButton.layoutX.value + (columnWidth + 20), removeColumnButton.layoutY.value)
          }
      }

      var removeColumnButton: Node = new Button("Remove last column") {
          onAction = _ => {
              if(!columnList.forall(c => c == archive)) {
                  def removed() = columnList.filterNot(c => c == archive ).maxBy(_.co.layoutX.value)
                  removed().deleteCards()                                //removing all cards from the column
                  removed().eraseColumn()                                //demoving the actual column
                  newColumnButton.relocate(newColumnButton.layoutX.value - (columnWidth + 20), newColumnButton.layoutY.value)
                  this.relocate(this.layoutX.value - (columnWidth + 20), this.layoutY.value)
              }
          }
      }


      val saveBoard = new Button("Save board") {
          onAction = _ => {
              if(isnewboard) updateBoardMenu()
              columnList = columnList.filter(_.co != archive.co)          //removing the archive column from the columnlist to avoid dublicates
              if(currentBoard != null ) currentBoard.get.menuBox.updateTitle((" " + currentTitle + " - Last edit: "+ Calendar.getInstance().getTime.toString.dropRight(9) + Calendar.getInstance().getTime.toString.takeRight(4)))
              showMenu()
          }
      }

      onMouseMoved = (me: MouseEvent) => {                                  //making the archive hide if not howered and making sure the boardmenu stays up to date
        if(dragActive || !archive.co.hover.value) {
            archive.hideCards()
            archive.co.toFront()
        }
      }

      //node positions
      title.relocate(5,10)                                   //top panel
      cardTypeLabel.relocate(250,0)
      cardTypeSelector.relocate(250, 20)
      cardColorLabel.relocate(350,0)
      cardColorSelector.relocate(350,20)
      tagFilterLabel.relocate(480,0)
      tagFilter.relocate(480,20)
      removeBox.relocate(140, 0)
      deleteBox.relocate(700, 0)
      archive.relocate(600, 15)
      saveBoard.relocate(700, 20)

      column0.co.relocate(5, columntopy)                    //columns
      newColumnButton.relocate(columnWidth+25, columntopy)
      removeColumnButton.relocate(columnWidth+25, columntopy + 45)

      children = Seq(archive.co, title, cardTypeLabel, cardTypeSelector, cardColorLabel, cardColorSelector, tagFilterLabel, tagFilter, newColumnButton, removeBox, deleteBox, saveBoard, detectonCircle, removeColumnButton)

      // startup checks based on if the board is new or an edit
      if(loadColumn.nonEmpty) loadColumn.foreach(c => if(!children.contains(c.co)) children.add(c.co))                                                                        //adds all saved columns
      if(loadCards.nonEmpty) loadCards.foreach(c => if(!children.contains(c.p)) c.p.addCustomCard(c) )                                                                   //adds all saved cards into the columns
      if(currentBoard.get.archivedCards.nonEmpty) currentBoard.get.archivedCards.foreach(c => cardArcive = cardArcive :+ c)                      //updating archive
      if(loadTitle != "") title.label.text = loadTitle                                                                                           //adds the title if such was saved
      if(isnewboard && !ispreset1) children.add(column0.co)                                                                                      //checking if we have a new board so we know if we should add the stock column
      else {
          columnList = columnList.filter(_ != column0).filter(_ != archive)                                                                      // if not remove it from the column list to avoid it being double saved
          newColumnButton.relocate(newColumnButton.layoutX.value + (columnList.length - 1) * (columnWidth + 20), newColumnButton.layoutY.value)  //moving along the column button to match the column amount
          removeColumnButton.relocate(removeColumnButton.layoutX.value + (columnList.length - 1) * (columnWidth + 20), removeColumnButton.layoutY.value)
      }

  }

  def mainBoard() = {

        new JFXApp.PrimaryStage {
            title = "Kabaan app"
            scene = new Scene(windowWidth,windowHeigth) {

                onMouseDragged = (me: MouseEvent) => {
                    dragActive = true                                                                                 //drag and drop initiator
                }
                onMouseMoved = (me: MouseEvent) => {
                   detectonCircle.relocate(me.x-detectonCircle.radius.value/2, me.y-detectonCircle.radius.value/2)    //drag and drop helper
                }

                root = new BorderPane() {
                    top = {
                        if(isnewboard && !ispreset1) {                                                                //if its a completely new board => load a blank board
                          panelsPane()
                        }
                        else {                                                                                        //else load it with data
                          panelsPane(columnList.filter(_.co != archive.co) , cardList, currentTitle)
                        }
                    }
                }

            }
       }
  }


  val loadBoardMenu = new VBox {
      prefWidth = 500
      prefHeight = 50

      val title = new Label("Boards")

      val create = new HBox {

          val loadNew = new Button("create board"){
              onAction = _ => {
                  createBoard()                                                    //creates and shows a new board
              }
          }
          val loadPreset = new Button("load preset"){
              onAction = _ => {
                 loadpreset1()
              }
          }

          children = Seq(loadNew, loadPreset)

      }
      style = GUI.border
      children = Seq(title, create)
  }

  def boardSelection() = new JFXApp.PrimaryStage {

      title = "Board menu"
      scene = new Scene(windowWidth, windowHeigth) {

          root = new BorderPane() {
              top = new Pane() {

                  onMouseMoved = (me: MouseEvent) => if(currentBoard.isDefined) resetBoard()

                  loadBoardMenu.relocate(10, 10)
                  children = loadBoardMenu
              }

          }

      }
  }


  stage = boardSelection()                        //selecting what to show in the GUI
  def showBoard() = stage = mainBoard()
  def showMenu() = stage = boardSelection()

  val preset1Title = "preset1"
  def preset1columns() = Seq(
         new Column("To do"){
        relocate(5, columntopy)
      }, new Column("Doing"){
        relocate((columnWidth+20) + 5, columntopy)
      }, new Column("Done"){
        relocate(2 * (columnWidth+20) + 5, columntopy)
      })

  def loadpreset1(): Unit = {
      ispreset1 = true
      createBoard()
      ispreset1 = false
  }



    //card types
  def newTextField(s: String = ""): TextField = {
    val f = new TextField() {
          prefWidth = columnWidth - 10
          prefHeight = 10
          text = s
    }
    f
  }

  def newTextarea(): Node = new TextArea() {
          prefWidth = columnWidth - 10
          prefHeight = 20
          promptText = "textarea"
  }

  def newSlider(): Node = new VBox {
          val text = newTextField()
          text.promptText = "Slidertext"
          val slider = new Slider() {
          prefHeight = 20
          }
          children = Seq(text, slider)
  }

  def newCheckBox() = {
      new VBox {
          //var tasks = Seq[CheckBox]()

          def addTask(s: String) = {
              val t: CheckBox = new CheckBox(s) {
                onKeyReleased = (key: KeyEvent) => {
                  key.code match {
                    case KeyCode.BackSpace => {
                      if(this.selected.value) deleteTask(this)
                    }
                    case _ =>
                  }
                }
              }
              children.add(t)
          }
          def deleteTask(t: CheckBox) = {
                children.remove(t)
          }
          val cardTitle = new TextField() {
              promptText = "main task"
          }
          val taskName = new TextField() {
             promptText = "sub task"

            onKeyReleased = (key: KeyEvent) => {
              key.code match {
                case KeyCode.Enter => {
                  addTask(text.value)
                  text = ""
                }
                case _ =>
              }
            }
          }
          children = Seq(cardTitle)

          onMouseEntered = (me: MouseEvent) => {
              if(!children.contains(taskName)) children.add(taskName)
          }
          onMouseExited = (me: MouseEvent) => {
              if(children.contains(taskName)) children.remove(taskName)
          }
      }
  }


    //card options
  def cardType(): Node = cardTypeSelector.value.value match {
     case "Field"    => val c = newTextField(); c.promptText = "cardtext"; c
     case "Slider"   => newSlider()
     case "Checkbox" => newCheckBox()
  }

  def  cardColor() = cardColorSelector.value.value



  //cards and column classes
  class Column(title: String = "", cards: Seq[Card] = Seq()) {

      if(columnList == null) columnList = Seq()
      columnList = columnList :+ this

      var co: VBox = new VBox(6) {  // card node
          prefWidth = columnWidth
          prefHeight = 50
          val header = newTextField(title)
          header.promptText = "title"
          val addCardButton = new Button("Add card") {
              onAction = _ => {
                  addCard()
              }
          }

          children = Seq(header, addCardButton)
          style = columnStyle()
      }

      if(cards.nonEmpty) cards.foreach(this.addCustomCard(_))         //helper for creating board

      //column functions
      def relocate(x: Double, y: Double) = co.relocate(x,y)           //relocates the column into a new position
      def addCard(): Unit = {                                         //adds a new card of the set type to the card
          val next = new Card(this, cardType())
          if(cardList == null) cardList = Seq()
          cardList = cardList :+ next                                 //and adds it to the column and global lists
          co.children.add(next.ca)
      }
      def addCustomCard(card: Card): Unit = {                         //adds a already existing card to the column
          if(!co.children.contains(card.ca)) co.children.add(card.ca)
          card.p = this
      }
      def removeCard(n: Node): Unit = {                                //removes the card from the column
          var removed = n
          if(co.children.map(_.getId).contains(n.getId)) {
            co.getChildren.remove(n)
          }
      }
      def deleteCards() = {
        cardList.foreach(c => if(c.p == this) c.eraseCard())
      }
      def eraseColumn() = {
        columnList = columnList.filter(_ != this)
        currentPane.get.children.remove(this.co)
      }

  }

    //drag and drop helpers
  private var nodeX:  Double = 0d
  private var nodeY:  Double = 0d


  class Card(parent: Column, data: Node = newTextField()) {

      var p: Column = parent
      var cardTag = ""

      var ca: VBox = new VBox(6) {
          prefWidth = columnWidth
          prefHeight = 50
          var d = data

          val tag = new Label("")
          val input = new TextField(){
              visible = true
              prefWidth = 100
              prefHeight = 20
              promptText = "tag"
          }
          tag.font = new Font("Cabria", 10)
          onMouseEntered = (me: MouseEvent) => {
              if(!children.contains(input)) children.add(input)
          }
          onMouseExited = (me: MouseEvent) => {
              children.remove(input)
          }
          input.text.onChange {
              tag.text = input.text.value
              cardTag = input.text.value
          }

          children = Seq(tag, d)
          style = cardStyle()

          //detect interaction for cards
          filterEvent(MouseEvent.Any) {
              me: MouseEvent =>
                  if(!dragActive)  {
                      me.eventType match {
                          case MouseEvent.MousePressed =>                                //sets a save state of the node origin
                              nodeX = -me.sceneX
                              nodeY = -me.sceneY
                              println(this.isFocused)
                          case _ =>
                      }
                  }
                  else if (dragActive) {
                      me.eventType match {
                          case MouseEvent.MouseDragged =>
                              this.parent.get().toFront()                                                                                 //move card and respective column to front to be visible
                              detectonCircle.relocate(me.sceneX-detectonCircle.radius.value/2, me.sceneY-detectonCircle.radius.value/2)   //intersection checker moved along
                              this.translateX = nodeX + me.sceneX                                                                         //start moving around the card
                              this.translateY = nodeY + me.sceneY
                              showBoxes()                                                                                                 //makes the removebox/deletebox visible to allow deleting of cards

                          case MouseEvent.MouseReleased =>
                              if(checkOverlapp().nonEmpty && dragActive){                                                                 //if a dragged card is dropped into an other column
                                  var goalColumn = columnList.find(a => a.co.toString.contains(checkOverlapp().head.toString))            //find goal column in question
                                  deleteCardFromParent()                                                                                            //remove card from the old parent column
                                  if(cardArcive.contains(Card.this) && goalColumn.isEmpty) cardArcive = cardArcive.filter(_ != Card.this) //if card already is in the archive and is removed it will be removed from the board completely
                                  else {
                                      cardArcive = cardArcive.filter(_ != Card.this)                                                      //removes card from archive so it can be added to its new parent
                                      if(goalColumn.isEmpty) cardArcive = cardArcive :+ Card.this                                         //if the parent is empty aka the remove box => add to archive
                                      else goalColumn.get.addCustomCard(Card.this)                                                        //else add to the new column
                                  }
                              }
                              undoDrag()                                                                                                   //move the card back to its correct place in the old/new column
                              dragActive = false                                                                                           //mouse released => drag not active
                              archive.hideCards()                                                                                          //hide archive in the case that we had interacted with it
                              hideBoxes()                                                                                                  //hide the remove and delete box
                          case _ =>
                      }
                      me.consume()                                                                                                          //reset mouse state
                  }
                  //drag amd drop overlapp checker
                  def checkOverlapp(): Seq[Node] = {
                      var i: Seq[Node] = Seq()                                                                                              //overlap holder
                      var columns = detectonCircle.parent.get().getChildrenUnmodifiable.filter(_.getClass.toString.contains("VBox"))        //search only for vboxes aka column
                      for(n <- columns) { //check intersections
                          if(n.boundsInParent.value.intersects(detectonCircle.boundsInParent.value)){                                       //check if the column in question intersects mouse
                            if(!this.parent.toString.contains(n.toString)) i = i :+ n                                                       //as the original column still is linked we filter it out before returning a potentially new column
                          }
                      }
                      i
                  }
          }
          //drag and drop extra features
          def undoDrag() = {                      //returns node back to its columnm position before drag
              this.translateX = 0
              this.translateY = 0
          }
      }
      //card functions
      def deleteCardFromParent(): Unit = p.removeCard(ca)   //removes the card from its parent to make it easier to move to new parents

      def eraseCard() = {
        deleteCardFromParent()
        cardList = cardList.filter(_ == this)
      }
  }

}
