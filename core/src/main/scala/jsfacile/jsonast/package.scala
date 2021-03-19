package jsfacile

import scala.{collection => generic}

import jsfacile.api.{Appender, FromJsonCharArrayConvertible, JsDocument, ParseError, Parser, parserOf}
import jsfacile.read.Skip

package object jsonast {

	sealed trait JsValue extends Any
	case class JsObject(fields: generic.Map[String, JsValue]) extends AnyVal with JsValue
	object JsObject {
		def apply(elems: (String, JsValue)*): JsObject = JsObject(generic.Map.from(elems))
	}
	case class JsArray(array: generic.Iterable[JsValue]) extends AnyVal with JsValue
	object JsArray {
		def apply(elems: JsValue*): JsArray = JsArray(elems)
	}
	case class JsString(string: String) extends AnyVal with JsValue
	case class JsNumber(number: BigDecimal) extends AnyVal with JsValue
	object JsBoolean {def apply(value: Boolean): JsBoolean = if (value) JsTrue else JsFalse}
	sealed trait JsBoolean extends JsValue {def value: Boolean}
	case object JsTrue extends JsBoolean {override def value = true}
	case object JsFalse extends JsBoolean {override def value = false}
	case object JsNull extends JsValue

	/** Contains a JSON document.
	 *
	 * Useful for fields that are already a JSON document. */
	case class JsDocument(value: String) extends AnyVal with JsValue {
		/** Tries to create an instance of the specified type with the value represented by this [[java.lang.String]] in JSON format.
		 *
		 * @tparam T the type of the instance to be created. This type parameter should be specified explicitly. */
		def fromJson[T](implicit pt: Parser[T]): Either[ParseError, T] = {
			new FromJsonCharArrayConvertible(value.toCharArray).fromJson[T](pt)
		}
	}

	//// Parsers ////

	implicit val jpAstNull: Parser[JsNull.type] = Parser.expectStr("null", JsNull)
	implicit val jpAstFalse: Parser[JsFalse.type] = Parser.expectStr("false", JsFalse)
	implicit val jpAstTrue: Parser[JsTrue.type] = Parser.expectStr("true", JsTrue)
	implicit val jpAstBoolean: Parser[JsBoolean] = { cursor =>
		if (cursor.comes("true")) JsTrue
		else if (cursor.comes("false")) JsFalse
		else {
			cursor.miss("A boolean was expected")
			JsFalse
		}
	}
	implicit val jpAstNumber: Parser[JsNumber] = parserOf[BigDecimal] ^^ JsNumber.apply
	implicit val jpAstString: Parser[JsString] = parserOf[String] ^^ JsString.apply
	implicit val jpAstArray: Parser[JsArray] = parserOf[generic.Iterable[JsValue]] ^^ JsArray.apply
	implicit val jpAstObject: Parser[JsObject] = parserOf[generic.Map[String, JsValue]] ^^ JsObject.apply

	implicit lazy val jpAstValue: Parser[JsValue] = Parser.pick >> { char =>
		val parser: Parser[_ <: JsValue] = char match {
			case '"' => jpAstString
			case '{' => jpAstObject
			case '[' => jpAstArray
			case 't' => jpAstTrue
			case 'f' => jpAstFalse
			case 'n' => jpAstNull
			case _ => jpAstNumber
		}
		parser.asInstanceOf[Parser[JsValue]]
	}

	implicit val jpJsDocument: Parser[JsDocument] = cursor => {
		val value = cursor.stringConsumedBy(Skip.jsValue)
		if (cursor.ok) {
			JsDocument(value)
		} else {
			cursor.miss("A JSON document was expected");
			Parser.ignored[JsDocument]
		}
	}

	//// Appenders ////

	implicit val jaAstNull: Appender[JsNull.type] = (r, _) => r.append("null");
	implicit val jaAstTrue: Appender[JsTrue.type] = (r, _) => r.append("true");
	implicit val jaAstFalse: Appender[JsFalse.type] = (r, _) => r.append("false");
	implicit val jaAstBoolean: Appender[JsBoolean] = (r, a) => r.append {if (a.value) "true" else "false"}
	implicit val jaAstNumber: Appender[JsNumber] = (r, a) => r.appendSummoned[BigDecimal](a.number)
	implicit val jaAstString: Appender[JsString] = (r, a) => r.appendSummoned[String](a.string)
	implicit val jaAstArray: Appender[JsArray] = (r, a) => r.appendSummoned[generic.Iterable[JsValue]](a.array)
	implicit val jaAstObject: Appender[JsObject] = (r, a) => r.appendSummoned[generic.Map[String, JsValue]](a.fields)
	implicit val jaJsDocument: Appender[JsDocument] = (r, jd) => r.append(jd.value);

	implicit lazy val jaAstValue: Appender[JsValue] = { (r, a) =>
		val appender: Appender[_ <: JsValue] = a match {
			case JsNull => jaAstNull
			case JsTrue => jaAstTrue
			case JsFalse => jaAstFalse
			case JsNumber(_) => jaAstNumber
			case JsString(_) => jaAstString
			case JsArray(_) => jaAstArray
			case JsObject(_) => jaAstObject
			case JsDocument(_) => jaJsDocument
		}
		appender.asInstanceOf[Appender[JsValue]].append(r, a)
	}


}
