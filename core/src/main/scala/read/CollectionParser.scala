package read

import scala.collection.mutable.Builder

object CollectionParser {
	trait CollectionParserHelper[C, E] {
		def elemValueParser: Parser[E];
		def collectionBuilder: Builder[E, C]
		val ignoreE: Parser.Ignore[E];
	}

	implicit def jpCollection[C <: Iterable[E], E](implicit helper: CollectionParserHelper[C, E]): Parser[C] = new CollectionParser[C, E](helper)
}

import CollectionParser._

class CollectionParser[C <: AnyRef, E](helper: CollectionParserHelper[C, E]) extends Parser[C] {
	import Parser._
	import SyntaxParsers.{coma, skipSpaces}

	private val collectionParser: Parser[C] = {
		implicit val ignoreE: Parser.Ignore[E] = helper.ignoreE;

		'[' ~> skipSpaces ~> (helper.elemValueParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, helper.collectionBuilder) <~ skipSpaces <~ ']'
	}

	override def parse(cursor: Parser.Cursor): C = collectionParser.parse(cursor)
}
