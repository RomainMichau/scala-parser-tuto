package com.rmichau.scalaparser.instances

import com.rmichau.scalaparser.Combinators._

object DateParser {

  val MONTHS = ANY_OF(
    Seq("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec").map(x =>
      IDENT(x.toLowerCase)
    )
  )
  val DOM        = ANY_OF((1 to 31).map(INT))
  val YEAR       = ANY_OF((1996 until 2002).map(INT))
  val /          = ANY_OF(Seq(SYMBOL("/"), SYMBOL("-")))
  val dateParser = SEQUENCE(Seq(DOM, /, MONTHS, /, YEAR))

  def parse = dateParser
}
