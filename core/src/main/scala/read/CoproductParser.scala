package read

import scala.reflect.runtime.{universe => ru}

import read.CoproductParserHelper.Coproduct

object CoproductParser {

	implicit def jpCoproduct[T <: Coproduct](implicit helper: CoproductParserHelper[T]): Parser[T] = new CoproductParser[T](helper)
}


class CoproductParser[T <: Coproduct](helper: CoproductParserHelper[T]) extends Parser[T] {
	override def parse(cursor: Parser.Cursor): Nothing = ???
}
