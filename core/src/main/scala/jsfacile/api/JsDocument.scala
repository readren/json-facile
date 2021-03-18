package jsfacile.api

import jsfacile.read.Skip

/** Contains a JSON document.
 *
 * Useful for fields that are already a JSON document. */
case class JsDocument(value: String) extends AnyVal {
	/** Tries to create an instance of the specified type with the value represented by this [[java.lang.String]] in JSON format.
	 *
	 * @tparam T the type of the instance to be created. This type parameter should be specified explicitly. */
	def fromJson[T](implicit pt: Parser[T]): Either[ParseError, T] = {
		new FromJsonCharArrayConvertible(value.toCharArray).fromJson[T](pt)
	}
}

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
