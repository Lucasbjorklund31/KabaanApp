import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.control._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Font


object GUI extends JFXApp {

  //Editgables
  //background variables
   var backgroundColor = "gray"

  //column variables
  var columnBackgroundColor = "white"
  var columnWidth = 150



  //Stock
  //background variables
  var ColumnList = Seq[Column]()
  var CardList = Seq[Card]()
  var cardArcive = Seq[Card]()
  var dragActive = false
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
    "-fx-background-color: " + panelsPane.cardColorSelector.value.value +
    border
  }

  var detectonCircle = new Circle { //helper for drag and drop function
        radius = 5
        mouseTransparent = true
        visible = false
  }

  //scene main components
  val panelsPane = new Pane() {

      maxWidth = 800
      maxHeight = 500

      //top panel
      val title = new HBox(){
          prefWidth = 150
          prefHeight = 30
          val label = new Label("Title")
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
      }

      val cardTypeLabel = new Label("Card type")
      val cardTypeSelector = new ComboBox(cardTypes)
      cardTypeSelector.onAction = (e: Any) => typeChanged = true

      val cardColorLabel = new Label("Card color")
      val cardColorSelector = new ComboBox(cardColors)
      cardColorSelector.onAction = (e: Any) => println(cardTypeSelector)

      val taggFilterLabel = new Label("Tagg filter")
      val taggFilter = new TextField(){
        prefWidth = 100
        prefHeight = 20
        promptText = "Card tagg filter"
      }
      taggFilter.text.onChange {
        CardList.foreach(a =>
        if(a.cardTagg == taggFilter.text.value) a.ca.visible = true
        else a.ca.visible = false)
      }

      //val cardArchiveLabel = ???
      //val cardArchive = ???


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
      val removeBox = new VBox {
          visible = false
          prefWidth = columnWidth
          prefHeight = 50
          val title = new Label("Delete Card")

          children = Seq(title)
          style = columnStyle()
      }
      val archive = new Column() {
        co.children.clear()
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

      title.relocate(5,10)
      cardTypeLabel.relocate(150,0)
      cardTypeSelector.relocate(150, 20)
      cardColorLabel.relocate(250,0)
      cardColorSelector.relocate(250,20)
      taggFilterLabel.relocate(380,0)
      taggFilter.relocate(380,20)
      removeBox.relocate(0, 0)
      archive.relocate(500, 15)

      column0.co.relocate(0, columntopy)
      newColumnButton.relocate(columnWidth+20,columntopy)
      detectonCircle.relocate(0,0)

      children = Seq(title, cardTypeLabel, cardTypeSelector, cardColorLabel, cardColorSelector, taggFilterLabel, taggFilter, column0.co, newColumnButton, removeBox, archive.co, detectonCircle)
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
          CardList = CardList :+ next
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




  //drag and drop helpers
   private var nodeX:  Double = 0d
   private var nodeY:  Double = 0d

   def cardType(): Node = panelsPane.cardTypeSelector.value.value match {
     case "field" => newTextField("textfield")
     case "area" => newTextarea()
     case "slider" => newSlider()
     case "checkbox" => newCheckBox()
   }

   def  cardColor() = panelsPane.cardColorSelector.value.value

  class Card(parent: Column, data: Node = newTextField("cardtext")) {

      CardList = CardList :+ this

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
                          detectonCircle.relocate(me.sceneX-detectonCircle.radius.value/2, me.sceneY-detectonCircle.radius.value/2)
                          dragActive = true
                          this.translateX = nodeX + me.sceneX
                          this.translateY = nodeY + me.sceneY
                          panelsPane.removeBox.visible = true
                      case MouseEvent.MouseReleased =>
                          if(checkOverlapp().nonEmpty && dragActive){
                              deleteThis()
                              panelsPane.archive.hideCards()
                              println(cardArcive.map(_.ca.toString))
                              println(this.toString)
                              cardArcive = cardArcive.filter(_ != Card.this)
                              var goalColumn = ColumnList.find(a => a.co.toString.contains(checkOverlapp().head.toString))
                              if(goalColumn.isEmpty) cardArcive = cardArcive :+ Card.this
                              else {
                                goalColumn.get.addCustomCard(Card.this)
                              }
                              this.undoDrag()
                          }
                          else undoDrag()
                          dragActive = false
                          panelsPane.removeBox.visible = false
                          println(cardArcive)
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
                println("cardarchive: " + cardArcive)
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

      def deleteThis(): Unit = p.removeCard(ca)

  }

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

}
