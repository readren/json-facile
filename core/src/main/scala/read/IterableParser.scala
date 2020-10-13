package read

import scala.collection.IterableFactory

import read.Parser.Ignore

object IterableParser {

	type LowerBound[E] = Iterable[E];

	implicit def jpIterable[CC[E] <: LowerBound[E], E](implicit parserE: Parser[E], factory: IterableFactory[CC], ignoreE: Ignore[E]): Parser[CC[E]] =
		new IterableParser[CC, E](parserE, factory)(ignoreE);

	implicit val listFactory: IterableFactory[List] = List
	implicit val setFactory: IterableFactory[Set] = Set
	implicit val seqFactory: IterableFactory[Seq] = Seq
}

import IterableParser._

class IterableParser[CC[E] <: LowerBound[E], E](parserE: Parser[E], factory: IterableFactory[CC])(implicit ignoreE: Ignore[E]) extends Parser[CC[E]] {
	import Parser._
	import SyntaxParsers.{coma, skipSpaces}

	private val collectionParser: Parser[CC[E]] = {

		'[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, factory.newBuilder[E]) <~ skipSpaces <~ ']'
	}

	override def parse(cursor: Parser.Cursor): CC[E] = collectionParser.parse(cursor)
}
