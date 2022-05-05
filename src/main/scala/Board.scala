import GUI.{Card, Column}
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, VBox}

class Board {

  var title: String = ""
  var lastEdited: String = ""
  var columns: Seq[Column] = Seq[Column]()
  var cards: Seq[Card] = Seq[Card]()

  val displayBox = new VBox {
      val name = new Label(title)
      var edit = new Label(lastEdited)

      children = Seq(name, edit)
  }
}
