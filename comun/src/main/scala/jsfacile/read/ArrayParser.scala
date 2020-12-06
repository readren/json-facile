package jsfacile.read

import scala.reflect.ClassTag

import jsfacile.read.Parser._

/**
 * @tparam E element's type */
class ArrayParser[E](parserE: Parser[E], ctE: ClassTag[E]) extends Parser[Array[E]] {

	assert(parserE != null);

	override def parse(cursor: Cursor): Array[E] = {
		if (cursor.isPointing) {
			if (cursor.pointedElem == '[') {
				cursor.advance();
				val builder = Array.newBuilder[E](ctE)
				var have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != ']') {
					have = false;
					val value = parserE.parse(cursor);
					if (cursor.consumeWhitespaces()) {
						builder.addOne(value);
						have = cursor.pointedElem == ']' || (cursor.consumeChar(',') && cursor.consumeWhitespaces());
					}
				}
				if (have) {
					cursor.advance();
					builder.result()
				} else {
					cursor.fail(s"Invalid syntax for Array.");
					ignored[Array[E]]
				}
			} else {
				cursor.miss(s"An array opening char was expected.")
				ignored[Array[E]]
			}
		} else {
			cursor.miss("An array opening char was expected but the end of the content was reached.")
			ignored[Array[E]]

		}

	}
}
