package api

import scala.collection.mutable.Buffer

class Column(id: Int) {

  val idVal = id
  var title = ""

  var cards: Buffer[Card] = Buffer()

}
