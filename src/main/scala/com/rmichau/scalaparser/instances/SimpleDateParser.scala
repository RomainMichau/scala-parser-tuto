package com.rmichau.scalaparser.instances

import com.rmichau.scalaparser.Combinators._

object DateParser {

  val MONTHS = ONE_OF(
    Seq("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec").map(x =>
      WORD_WS(x.toLowerCase)
    )
  )
  val DOM        = ONE_OF((1 to 31).reverse.map(_.toString).map(WORD_WS))
  val YEAR       = ONE_OF((1996 until 2002).map(_.toString).map(WORD_WS))
  val /          = ONE_OF(Seq(WORD_WS("/"), WORD_WS("-")))
  val dateParser = SEQ(DOM, /, MONTHS, /, YEAR)

  def parse = dateParser
}
