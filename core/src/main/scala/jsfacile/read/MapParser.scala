package jsfacile.read

import scala.collection.mutable

import jsfacile.read.MapParser.MapUpperBound
import jsfacile.read.Parser._
import jsfacile.read.SyntaxParsers._


object MapParser {

	type MapUpperBound[K, V] = scala.collection.Map[K, V];

	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V]

	def apply[M <: MapUpperBound[K, V], K, V](
		parserK: Parser[K],
		parserV: Parser[V],
		builderCtor: () => mutable.Builder[(K, V), M]
	): MapParser[M, K, V] =
		new MapParser(parserK, parserV, builderCtor)
}

class MapParser[M <: MapUpperBound[K, V], K, V](
	parserK: Parser[K],
	parserV: Parser[V],
	builderCtor: () => mutable.Builder[(K, V), M]
) extends Parser[M] {
	override def parse(cursor: Cursor): M = {
		if (cursor.have) {
			val builder = builderCtor();
			val firstElem = cursor.pointedElem;
			var ok = false;
			if (firstElem == '[') {
				cursor.advance();
				ok = cursor.consumeWhitespaces();
				while (ok && cursor.pointedElem != ']') {
					ok = false;
					if (cursor.consumeChar('[') &&
						cursor.consumeWhitespaces()
					) {
						val key = parserK.parse(cursor);
						if (cursor.consumeWhitespaces() &&
							cursor.consumeChar(',') &&
							cursor.consumeWhitespaces()
						) {
							val value = parserV.parse(cursor);
							if (cursor.consumeWhitespaces() &&
								cursor.consumeChar(']') &&
								cursor.consumeWhitespaces()
							) {
								builder.addOne(key -> value);
								ok = cursor.pointedElem == ']' || (cursor.consumeChar(',') && cursor.consumeWhitespaces());
							}
						}
					}
				}

			} else if (firstElem == '{') {
				cursor.advance();
				ok = cursor.consumeWhitespaces();
				while (ok && cursor.pointedElem != '}') {
					ok = false;
					val keyStr = string.parse(cursor);
					val key =
						if (parserK == PrimitiveParsers.jpString) {
							keyStr.asInstanceOf[K]
						} else {
							val keyCursor = new CursorStr(keyStr);
							val key = parserK.parse(cursor);
							if (!keyCursor.ok || keyCursor.isPointing) {
								cursor.fail(s"The map key parser failed to interpret the key embedded in the json field name.")
							}
							key
						}
					if (cursor.consumeWhitespaces() &&
						cursor.consumeChar(':') &&
						cursor.consumeWhitespaces()
					) {
						val value = parserV.parse(cursor);
						if (cursor.consumeWhitespaces()) {
							builder.addOne(key -> value);
							ok = cursor.pointedElem == '}' || (cursor.consumeChar(',') && cursor.consumeWhitespaces())
						}
					}
				}
			} else {
				cursor.fail(s"A map opening char was expected but '${cursor.pointedElem}' was found")
			}

			if (ok) {
				cursor.advance();
				builder.result()
			} else {
				cursor.fail("Invalid syntax for map");
				ignored[M]
			}

		} else {
			cursor.fail("A map opening char was expected but the end of the content was reached")
			ignored[M]
		}
	}

}
