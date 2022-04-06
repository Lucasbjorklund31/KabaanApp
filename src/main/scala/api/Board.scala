package api

import scala.collection.mutable.Buffer
import scala.collection.immutable.ArraySeq


 abstract class Board {

  //basic variables
  var title = ""
  var columns: Buffer[Column] = Buffer()
  var cards: Buffer[Card] = Buffer()
  var idCount = 0 //id for new card/column

  // trackers for which element on the board is currently being interacted with
  // if selected keyboard inputs will edit the text
  // if draged and helt n will temporarly move with the mouse to a new location
  // => if valid place, it will have its place updated
  // => if invalid, return to original place
  var selectedCard: Option[Card] = None
  var selectedColumn: Option[Column] = None

  //setting up basic functions

  //add
  def addColumn = columns += new Column(idCount) ; idCount += 1
  def addCard = cards += new Card(idCount) ; idCount += 1

  //remove
  def removeColumn(id: Int) = columns.filter(_.idVal == id)
  def removeCard(id: Int) = cards.filter(_.idVal == id)

  def update(input: String) = {
  if(selectedCard.isDefined) selectedCard.get.text
  else if(selectedColumn.isDefined) selectedColumn.get.title
  }




}
