
//import GUI.{borderStyle, makeDraggable}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.control.{Button, CheckBox, ProgressBar, TextField}
import scalafx.scene.input.{MouseDragEvent, MouseEvent}
import scalafx.scene.layout.{BorderPane, Pane, VBox}
import scalafx.scene.shape.Circle
import scalafx.scene.{Node, Scene}


object DragAndDropTest extends JFXApp{

  private val borderStyle            = "" +
    "-fx-background-color: white;" +
    "-fx-border-color: black;" +
    "-fx-border-width: 1;" +
    "-fx-border-radius: 5;" +
    "-fx-padding: 6;"

  def stage1(): JFXApp.PrimaryStage = new JFXApp.PrimaryStage {

    System.currentTimeMillis()

    title = "Drag and drop test"
    scene = new Scene(800,600) {



      //intersection
      root = new BorderPane() {
          top = panelsPane
      }
    }
  }

  val panelsPane = new Pane() {
      maxWidth = 800
      maxHeight = 500

      val progressBar = new ProgressBar {
        maxWidth = 300
        maxHeight = 10
        visible = true
        progress = 0.60
      }

      val switch = new Button("switch") {
        onAction = _ => stage = stage2()
      }
      switch.relocate(100, 100)
      progressBar.relocate(250, 250)

      children = Seq(switch, progressBar)
  }

  def stage2(): JFXApp.PrimaryStage = new JFXApp.PrimaryStage {

    System.currentTimeMillis()

    title = "Drag and drop test2"
    scene = new Scene(800,600) {



      //intersection
      root = new BorderPane() {
          top = panelsPane2
      }
    }
  }

  val panelsPane2 = new Pane() {
      maxWidth = 800
      maxHeight = 500

      val switch = new Button("switch") {
        onAction = _ => stage = stage1()
      }
      switch.relocate(200, 200)

      children = Seq(switch)
  }

  stage = stage1()

}

/*
   private var startTimeMillis = System.currentTimeMillis()
  private var endTimeMillis = System.currentTimeMillis()

  class Clocker(initial :Long, increment :Long, interval :Long) {
  private val start = LocalTime.now()
  def get :Long =
    initial + SECONDS.between(start, LocalTime.now()) / interval * increment
  }
  val clkr = new Clocker(0, 1, 1)
   */


