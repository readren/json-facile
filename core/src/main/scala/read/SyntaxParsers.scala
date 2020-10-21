package read

import scala.collection.mutable

/** Both the methods of this object and the [[Parser]]s given by them are thread safe (or should be). */
object SyntaxParsers {
	import Parser._

	//	def apply[T](implicit inter: LectorJson[T]): LectorJson[T] = inter;

	class MissingFieldException(className: String, fieldName: String) extends RuntimeException(s"class: $className, field: $fieldName")

	class CodePointStrBuilder extends mutable.Builder[Int, String] {
		private val sb = new java.lang.StringBuilder()
		override def clear(): Unit = sb.setLength(0);
		override def result(): String = sb.toString;
		override def addOne(elem: Int): CodePointStrBuilder.this.type = {
			sb.appendCodePoint(elem);
			this;
		};
	}

	val skipSpaces: Parser[Pos] = { cursor =>
		while (Character.isWhitespace(cursor.pointedElem)) {
			cursor.advance()
		}
		cursor.pos
	}
	val colon: Parser[Elem] = ':'
	val coma: Parser[Elem] = ','

	private def normalChar: Parser[Elem] = acceptElemIf(c => c != '\"' && c != '\\' && c >= 0x20 && c <= 0x10ffff)
	private def hexDigit: Parser[Elem] = acceptElemIf(c => ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'))
	private def hexCode: Parser[Elem] = 'u' ~> hexDigit.repNGen(4, () => new CodePointStrBuilder) ^^ { hex => Integer.parseInt(hex, 16) }
	private def escape: Parser[Elem] = '\\' ~> collect {
		case '"' => '"'.toInt
		case '\\' => '\\'.toInt
		case '/' => '/'.toInt
		case 'b' => '\b'.toInt
		case 'f' => '\f'.toInt
		case 'n' => '\n'.toInt
		case 'r' => '\r'.toInt
		case 't' => '\t'.toInt
	} | hexCode
	val string: Parser[String] = '\"' ~> (normalChar | escape).repGen { () => new CodePointStrBuilder } <~ '\"'

	private val digit: Parser[Elem] = acceptElemIf(Character.isDigit)
	private val skipDigits: Parser[Pos] = { cursor =>
		while (Character.isDigit(cursor.pointedElem)) {
			cursor.advance();
		}
		cursor.pos
	}
	private val digit19: Parser[Elem] = acceptElemIf(e => '1' <= e && e <= '9')
	private val skipInteger: Parser[Pos] = '-'.opt ~> (acceptChar('0') ~> pos) | (digit19 ~> skipDigits)
	private val skipFraction: Parser[Pos] = '.' ~> digit ~> skipDigits
	private val skipExponent: Parser[Pos] = ('e' | 'E') ~> ('+' | '-').opt ~> digit ~> skipDigits // me parece que en lugar de "digit" debería ser "digit19", pero me regí por la página https://www.json.org/json-en.html
	val skipJsNumber: Parser[Pos] = skipInteger ~> skipFraction.opt ~> skipExponent.opt ~> pos

	private val skipJsNull: Parser[Pos] = "null" ~> pos

	private val skipJsBoolean: Parser[Pos] = ("true" | "false") ~> pos

	private val skipJsString: Parser[Pos] = string ~> pos

	private def skipJsArray: Parser[Pos] = '[' ~> skipSpaces ~> (skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> ']' ~> pos

	private def skipJsObject: Parser[Pos] = '{' ~> skipSpaces ~> (string ~> skipSpaces ~> colon ~> skipSpaces ~> skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> '}' ~> pos

	val skipJsValue: Parser[Pos] = new Parser[Pos] {
		// I don't like lazy vals because they block the thread and that is not necessary here because in the worst case the value is initialized twice.
		var skipObject: Parser[Pos] = _;
		var skipArray: Parser[Pos] = _;
		override def parse(cursor: Cursor): Pos = {
			if (cursor.ok) {
				val p = cursor.pointedElem match {
					case '"' => skipJsString
					case '{' =>
						if(skipObject == null) {
							skipObject = skipJsObject
						}
						skipObject
					case '[' =>
						if(skipArray == null) {
							skipArray = skipJsArray
						}
						skipArray
					case 't' | 'f' => skipJsBoolean
					case 'n' => skipJsNull
					case _ => skipJsNumber.orFail("Invalid json value format.")
				}
				p.parse(cursor)
			} else {
				cursor.pos
			}
		}
	}
}
