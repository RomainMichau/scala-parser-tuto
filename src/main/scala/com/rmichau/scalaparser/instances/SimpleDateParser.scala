package com.rmichau.scalaparser.instances

import com.rmichau.scalaparser.Combinators._

object DateParser {

  val MONTHS = ANY_OF(
    Seq("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec").map(x =>
      WORD_WS(x.toLowerCase)
    )
  )
  val DOM        = ANY_OF((1 to 31).reverse.map(_.toString).map(WORD_WS))
  val YEAR       = ANY_OF((1996 until 2002).map(_.toString).map(WORD_WS))
  val /          = ANY_OF(Seq(WORD_WS("/"), WORD_WS("-")))
  val dateParser = SEQ(DOM, /, MONTHS, /, YEAR)

  def parse = dateParser
}
