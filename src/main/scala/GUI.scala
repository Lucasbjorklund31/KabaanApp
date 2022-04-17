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
import scalafx.scene.Scene
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
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.stage.{WindowEvent, StageStyle}

import scalafx.Includes._
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

  private val dragModeActiveProperty = new BooleanProperty(this, "dragModeActive", true)
  private val borderStyle            = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 6;" +
    "-fx-padding: 6;"

    stage = new JFXApp.PrimaryStage() {

      val panelsPane = new Pane() {
        val confirmationPanel = makeDraggable(createConfirmationPanel())
        val starterColumn     = makeDraggable(createColumn())

        confirmationPanel.relocate(0, 67)
        starterColumn.relocate(0, 106)

        children = Seq(confirmationPanel, starterColumn)
        alignmentInParent = Pos.TopLeft
      }

      val dragModeCheckbox = new CheckBox("Drag mode") {
        margin = Insets(6)
        selected = dragModeActiveProperty()
      }

      dragModeActiveProperty <== dragModeCheckbox.selected

      title = "Draggable Panels Example"
      scene = new Scene(400, 300) {
        root = new BorderPane() {
          center = panelsPane
          bottom = dragModeCheckbox
        }
      }
    }



  private def makeDraggable(node: Node): Node = {

    val dragContext = new DragContext()

    new Group(node) {
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
          if (dragModeActiveProperty()) {
            me.eventType match {
              case MouseEvent.MousePressed =>
                dragContext.mouseAnchorX = me.x
                dragContext.mouseAnchorY = me.y
                dragContext.initialTranslateX = node.translateX()
                dragContext.initialTranslateY = node.translateY()
              case MouseEvent.MouseDragged =>
                node.translateX = dragContext.initialTranslateX + me.x - dragContext.mouseAnchorX
                node.translateY = dragContext.initialTranslateY + me.y - dragContext.mouseAnchorY
              case _ =>
            }
            me.consume()
          }
      }
    }
  }

  /*
    private def makeDraggable(node: Node): Node = {

    val dragContext = new DragContext()

    new Group(node) {
      filterEvent(MouseEvent.Any) {
        me: MouseEvent =>
            me.eventType match {
              case MouseEvent.MousePressed =>
                dragContext.mouseAnchorX = me.x
                dragContext.mouseAnchorY = me.y
                dragContext.initialTranslateX = node.translateX()
                dragContext.initialTranslateY = node.translateY()
              case MouseEvent.MouseDragged =>
                node.translateX = dragContext.initialTranslateX + me.x - dragContext.mouseAnchorX
                node.translateY = dragContext.initialTranslateY + me.y - dragContext.mouseAnchorY
              case MouseEvent.MouseReleased =>
                if(dragContext.mouseAnchorX == me.x && dragContext.mouseAnchorY == me.y) {
                  println("node clicked")
                  println(node)
                }
              case _ =>
            }
            me.consume()
      }
    }
  }
   */

  private def createColumn(): Node = new VBox(6) {
    prefWidth = 100
    prefHeight = 250
    val title = new Label("Column")
    val cardText = new TextField() {
      prefColumnCount = 10
      promptText = "Input"
    }

    def card() = {
      new TextField() {
      prefColumnCount = 10
      promptText = "Input"
      }
    }

    val addCardButton: Button = new Button("Add card") {
        onAction = _ => {
        }
      }

    /*
              var cardcount = children.length - 1
          children = Seq(title)
          for(i <- 0 until cardcount) addCard()
          children += addCardButton
     */
    /*
    def addCard() = {
      children += new TextField() {
      prefColumnCount = 10
      promptText = "Input"
      }
    }
     */

    //def addCardButton() =
    children = Seq(
      title,
      cardText,
      addCardButton
    )
    //alignment = Pos.CenterRight
    style = borderStyle
  }

  private def createLoginPanel(): Node = {
    val toggleGroup1 = new ToggleGroup()

    val textField = new TextField() {
      prefColumnCount = 10
      promptText = "Your name"
    }

    val passwordField = new PasswordField() {
      prefColumnCount = 10
      promptText = "Your password"
    }

    val choiceBox = new ChoiceBox[String](
      ObservableBuffer(
        "English",
        "\u0420\u0443\u0441\u0441\u043a\u0438\u0439",
        "Fran\u00E7ais"
        )
      ) {
      tooltip = Tooltip("Your language")
      selectionModel().select(0)
    }

    new HBox(6) {
      children = Seq(
        new VBox(2) {
          children = Seq(
            new RadioButton("High") {
              toggleGroup = toggleGroup1
              selected = true
            },
            new RadioButton("Medium") {
              toggleGroup = toggleGroup1
              selected = false
            },
            new RadioButton("Low") {
              toggleGroup = toggleGroup1
              selected = false
            }
          )
        },
        new VBox(2) {
          children = Seq(textField, passwordField)
        },
        choiceBox
      )

      alignment = Pos.BottomLeft
      style = borderStyle
    }
  }

  private def createConfirmationPanel(): Node = new HBox(6) {
    val acceptanceLabel = new Label("Not Available")
    children = Seq(
      new Button("Accept") {
        onAction = _ => {
          acceptanceLabel.text = "Accepted"
        }
      },
      new Button("Decline") {
        onAction = _ => acceptanceLabel.text = "Declined"
      },
      acceptanceLabel
    )
    alignment = Pos.CenterLeft
    style = borderStyle
  }

  private def createProgressPanel(): Node = new HBox(6) {
    val slider = new Slider()
    val progressIndicator = new ProgressIndicator() {
      progress <== slider.value / slider.max
    }
    children = Seq(new Label("Progress:"), slider, progressIndicator)
    style = borderStyle
  }

  private final class DragContext {
    var mouseAnchorX     : Double = 0d
    var mouseAnchorY     : Double = 0d
    var initialTranslateX: Double = 0d
    var initialTranslateY: Double = 0d
  }



 
  /*
stage = new JFXApp.PrimaryStage {
    title = "Kabaan app"
    width = 1000
    height = 800
    /*
    scene = new Scene {
          fill = Black
          content = new Rectangle {
            x = 0
            y = 0
            width = 1000
            height = 40
            fill <== when(hover) choose Pink otherwise Red
          }
    }
     */
}

  val root = new GridPane
  val board = new Scene(root)

  stage.scene = board
  board.fill = Black

  val topLeft = new HBox
  val topRight = new HBox
  val c0 = new VBox
  val c1 = new VBox
  val c2 = new VBox
  val c3 = new VBox
  val c4 = new VBox


  root.add(topLeft, 0, 0)
  root.add(topRight, 1, 0, 4, 1)
  root.add(c0, 0, 1, 4, 1)
  root.add(c1, 1, 1, 4, 1)
  root.add(c2, 2, 1, 4, 1)
  root.add(c3, 3, 1, 4, 1)
  root.add(c4, 4, 1, 4, 1)

  val column0 = new ColumnConstraints
  val column1 = new ColumnConstraints
  val column2 = new ColumnConstraints
  val column3 = new ColumnConstraints
  val column4 = new ColumnConstraints

  val row0 = new RowConstraints
  val row1 = new RowConstraints

  column0.percentWidth = 20
  column1.percentWidth = 20
  column2.percentWidth = 20
  column3.percentWidth = 20
  column4.percentWidth = 20

  row0.percentHeight = 10
  row1.percentHeight = 90

  root.rowConstraints = Array[RowConstraints](row0, row1)
  root.columnConstraints = Array[ColumnConstraints](column0, column1, column2, column3, column4)

  topLeft.background = new Background(Array(new BackgroundFill((Blue), CornerRadii.Empty, Insets.Empty)))
  topRight.background = new Background(Array(new BackgroundFill((LightCyan), CornerRadii.Empty, Insets.Empty)))
  c0.background = new Background(Array(new BackgroundFill((Red), CornerRadii.Empty, Insets.Empty)))
  c1.background = new Background(Array(new BackgroundFill((Blue), CornerRadii.Empty, Insets.Empty)))
  c2.background = new Background(Array(new BackgroundFill((Pink), CornerRadii.Empty, Insets.Empty)))
  c3.background = new Background(Array(new BackgroundFill((Green), CornerRadii.Empty, Insets.Empty)))
  c4.background = new Background(Array(new BackgroundFill((Orange), CornerRadii.Empty, Insets.Empty)))

  val title = new Canvas(stage.width.value * column0.percentWidth.value / 100, stage.height.value * row0.percentHeight.value / 100)
  val g = title.graphicsContext2D
  g.fill = White
  g.font = new Font(30)
  g.fillText("Title", 10, 50)
  topLeft.children += title

  private var anchorPt: Point2D = null
  private var previousLocation: Point2D = null

  // Initialize stage to be movable via mouse
  initMovablePlayer()

  /**
   * Initialize the stage to allow the mouse cursor to move the application
   * using dragging.
   */
  private def initMovablePlayer(): Unit = {
    val scene = stage.getScene

    scene.onMousePressed = (event: MouseEvent) => anchorPt = new Point2D(event.screenX, event.screenY)

    scene.onMouseDragged = (event: MouseEvent) =>
      if (anchorPt != null && previousLocation != null) {
        stage.x = previousLocation.x + event.screenX - anchorPt.x
        stage.y = previousLocation.y + event.screenY - anchorPt.y
      }

    scene.onMouseReleased = (event: MouseEvent) => previousLocation = new Point2D(stage.getX, stage.getY)

    stage.onShown = (event: WindowEvent) => previousLocation = new Point2D(stage.getX, stage.getY)
  }
  */

}
