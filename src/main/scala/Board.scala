import GUI.{Card, Column, currentBoard, showBoard}
import scalafx.application.JFXApp
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.text.Font
/*
class Board extends JFXApp {

  var title: String = ""
  var lastEdited: String = ""
  var columns: Seq[Column] = Seq[Column]()
  var cards: Seq[Card] = Seq[Card]()

  val displayBox = new HBox {
      val name = new Label(title){
        visible = true
        font = new Font("Cabria", 15)
      }
      var ledit = new Label(lastEdited){
        visible = true
        font = new Font("Cabria", 15)
      }
      var edit = new Button("Edit board") {
        onAction = _ => currentBoard = Some(Board.this)
        showBoard()
      }

      children = Seq(name, edit)
      style = GUI.border
  }
}
*/