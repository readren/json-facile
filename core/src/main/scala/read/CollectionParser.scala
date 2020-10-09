package read

import scala.collection.mutable.Builder

object CollectionParser {
	trait CollectionReaderHelper[C, E] {
		def elemValueParser: Parser[E];
		def collectionBuilder: Builder[E, C]
		val ignoreE: Parser.Ignore[E];
		val ignoreC: Parser.Ignore[C];
	}
}

import CollectionParser._

class CollectionParser[C <: AnyRef, E](helper: CollectionReaderHelper[C, E]) extends Parser[C] {
	import JsonParsers._

	implicit def ignoreE: Parser.Ignore[E] = helper.ignoreE;
	implicit def ignoreC: Parser.Ignore[C] = helper.ignoreC;

	private def collectionParser: Parser[C] =
		skipSpaces ~> '[' ~> (skipSpaces ~> helper.elemValueParser).repSepGen(coma, helper.collectionBuilder) <~ ']'

	override def parse(puntero: Parser.Cursor): C = collectionParser.parse(puntero)
}
