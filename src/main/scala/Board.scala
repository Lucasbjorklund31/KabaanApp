import GUI.{Card, Column}
import scalafx.scene.layout.HBox

class Board {
  val displayBox = new HBox {

  }
  var title: String = ""
  var columns: Seq[Column] = Seq[Column]()
  var cards: Seq[Card] = Seq[Card]()
}
