package jsfacile.api

import jsfacile.read.{Parser, Skip}
import jsfacile.write.Appender

/** Contains a JSON document.
 *
 * Useful for fields that are already a JSON document. */
case class JsDocument(value: String) extends AnyVal

object JsDocument {

	implicit val appender: Appender[JsDocument] = (r, jd) => r.append(jd.value);

	implicit val parser: Parser[JsDocument] = cursor => {
		val value = cursor.stringConsumedBy(Skip.jsValue)
		if (cursor.ok) {
			JsDocument(value)
		} else {
			cursor.miss("A JSON document was expected");
			Parser.ignored[JsDocument]
		}
	}
}
