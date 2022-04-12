import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Font
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.scene.control.CheckBox
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.StackPane
import scalafx.Includes._
import scalafx.application.JFXApp
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
import scalafx.scene.paint.Color._

import api.Card
import api.Column
import api.Board



object GUI extends JFXApp {

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
  val topRight = new HBox()

  root.add(topLeft, 0, 0)
  root.add(topRight, 1, 0, 4, 1)

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



  val title = new Canvas(stage.width.value * column0.percentWidth.value / 100, stage.height.value * row0.percentHeight.value / 100)
  val g = title.graphicsContext2D
  g.fill = White
  g.font = new Font(30)
  g.fillText("Title", 10, 50)
  topLeft.children += title

}
