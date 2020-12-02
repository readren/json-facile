package jsfacile.read

import scala.collection.mutable

import jsfacile.joint.MapUpperBound
import jsfacile.read.Parser._

class MapParser[M <: MapUpperBound[K, V], K, V](
	parserK: Parser[K],
	parserV: Parser[V],
	parserString: Parser[String],
	builderCtor: () => mutable.Builder[(K, V), M]
) extends Parser[M] {
	override def parse(cursor: Cursor): M = {
		if (cursor.have) {
			val builder = builderCtor();
			val firstElem = cursor.pointedElem;
			var have = false;
			if (firstElem == '[') {
				cursor.advance();
				have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != ']') {
					have = false;
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
								have = cursor.pointedElem == ']' || (cursor.consumeChar(',') && cursor.consumeWhitespaces());
							}
						}
					}
				}

			} else if (firstElem == '{') {
				cursor.advance();
				have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != '}') {
					have = false;
					val keyStr = parserString.parse(cursor);
					if(cursor.ok) {
						val key =
							if (parserK == parserString) {
								keyStr.asInstanceOf[K]
							} else {
								val keyCursor = new CursorStr(keyStr);
								val key = parserK.parse(keyCursor);
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
								have = cursor.pointedElem == '}' || (cursor.consumeChar(',') && cursor.consumeWhitespaces())
							}
						}
					}
				}
			} else {
				cursor.miss(s"A map opening char was expected.")
			}

			if (have) {
				cursor.advance();
				builder.result()
			} else {
				cursor.fail("Invalid syntax for map");
				ignored[M]
			}

		} else {
			cursor.miss("A map opening char was expected but the end of the content was reached")
			ignored[M]
		}
	}

}
