package jsfacile.read

import scala.collection.mutable

import jsfacile.read.Parser._
import jsfacile.read.SyntaxParsers._


object MapParser {

	type MapUpperBound[K, V] = scala.collection.Map[K, V];

	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V]

	def apply[M <: MapUpperBound[K, V], K, V](
		parserK: Parser[K],
		parserV: Parser[V],
		builderCtor: () => mutable.Builder[(K, V), M]
	): Parser[M] = { cursor =>

		if (cursor.pointedElem == '[') {
			cursor.advance();
			val pairParser = '[' ~> skipSpaces ~> (parserK <~ skipSpaces) ~ (coma ~> skipSpaces ~> parserV <~ skipSpaces) <~ ']' ^^ { x => (x._1, x._2) };
			val asArray = skipSpaces ~> (pairParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, builderCtor) <~ skipSpaces <~ ']';
			asArray.parse(cursor)

		} else if (cursor.pointedElem == '{') {
			cursor.advance();
			val entryParser = ((string <~ skipSpaces <~ colon <~ skipSpaces) ~ (parserV <~ skipSpaces)).flatMap[(K, V)] { case keyStr ~ value =>
				if (parserK == PrimitiveParsers.jpString) {
					hit(keyStr.asInstanceOf[K] -> value)
				} else {
					val cursor = new CursorStr(keyStr);
					val key = parserK.parse(cursor);
					if (cursor.ok && cursor.atEnd) hit(key -> value)
					else fail(s"The map key parser failed to interpret the key embedded in the field name.")
				}
			}
			val asObject = skipSpaces ~> entryParser.repSepGen[Pos, M](coma <~ skipSpaces, builderCtor) <~ skipSpaces <~ '}';
			asObject.parse(cursor)

		} else {
			cursor.fail("Invalid syntax for a map.");
			ignored[M]
		}
	}
}
