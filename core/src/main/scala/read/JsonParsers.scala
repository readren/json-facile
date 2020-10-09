package read

import scala.collection.mutable

/** Both the methods of this object and the [[Parser]]s given by them are thread safe (or should be). */
object JsonParsers {
	import Parser._

	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;


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

	private def skipJsNumber: Parser[Pos] = skipInteger ~> skipFraction.opt ~> skipExponent.opt ~> pos
	private def skipJsNull: Parser[Pos] = "null" ~> pos
	private def skipJsBoolean: Parser[Pos] = ("true" | "false") ~> pos
	private def skipJsString: Parser[Pos] = string ~> pos
	private def skipJsArray: Parser[Pos] = '[' ~> skipSpaces ~> (skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> ']' ~> pos
	private def skipJsObject: Parser[Pos] = '{' ~> skipSpaces ~> (string ~> skipSpaces ~> colon ~> skipSpaces ~> skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> '}' ~> pos
	val skipJsValue: Parser[Pos] = new Parser[Pos] {
		private var mem: Parser[Pos] = _
		override def parse(cursor: Cursor): Pos = {
			if (mem == null) {
				this.mem = skipJsString | (skipJsArray | (skipJsObject | (skipJsNull | (skipJsBoolean | skipJsNumber))))
			}
			this.mem.parse(cursor)
		}
	}

	trait LectoresJsonLowPriority {

//		implicit def materializeGuia[T <: AnyRef]: GuiaLectorProducto[T] = new GuiaLectorProducto[T] {
//			override val className: String = "Borrame"
//			override val infoCampos: ListMap[String, GuiaLectorProducto.InfoCampo[_]] = ListMap.empty
//			override def crear(args: Seq[Any]): T = ???
//		}

		/** Invocador de instancias de [[ProductParser]] */
		implicit def jrClass[C <: AnyRef]: Parser[C] = new ProductParser[C]

	}
}


/**  Interpretadores para tipos nativos de scala */
trait JsonParsers extends JsonParsers.LectoresJsonLowPriority {
	import JsonParsers._

	implicit val jpString: Parser[String] = JsonParsers.string;

	/** Interpretador de Int en Json */
	implicit val jpInt: Parser[Int] = { cursor =>
		if (cursor.ok) {
			cursor.attempt { () =>
				var accum: Int = 0;
				var limit = 9; // mantissa length
				var pointedElem = cursor.pointedElem;
				val isNegative = pointedElem == '-';
				if (isNegative) {
					cursor.advance()
					pointedElem = cursor.pointedElem;
				};
				var digit = pointedElem - '0';
				if (digit < 0 || 9 < digit) {
					cursor.miss();
				} else {
					do {
						accum = accum * 10 + digit;
						limit -= 1;
						cursor.advance();
						pointedElem = cursor.pointedElem;
						digit = pointedElem - '0';
					} while (0 <= digit && digit <= 9 && limit > 0);

					if (0 <= digit && digit <= 9) {
						if (accum > MAX_INT_DIV_10 || (accum == MAX_INT_DIV_10 && (digit == 9 || !isNegative && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
							cursor.miss();
						} else {
							accum = accum * 10 + digit
							cursor.advance();
							pointedElem = cursor.pointedElem;
							if ('0' <= pointedElem && pointedElem <= '9') {
								cursor.miss();
							}
						}
					}
				}
				if (isNegative) -accum
				else accum
			}
		} else {
			0
		}
	}

	/** Interpretador de Long en Json */
	implicit val jpLong: Parser[Long] = { cursor =>
		if (cursor.ok) {
			cursor.attempt { () =>
				var accum: Long = 0;
				var limit = 18; // mantissa length

				var pointedElem = cursor.pointedElem;
				val isNegative = pointedElem == '-';
				if (isNegative) {
					cursor.advance()
					pointedElem = cursor.pointedElem;
				};
				var digit = pointedElem - '0';
				if (digit < 0 || 9 < digit) {
					cursor.miss();
				} else {
					do {
						accum = accum * 10L + digit;
						limit -= 1;
						cursor.advance();
						pointedElem = cursor.pointedElem;
						digit = pointedElem - '0';
					} while (0 <= digit && digit <= 9 && limit > 0);

					if (0 <= digit && digit <= 9) {
						if (accum > MAX_LONG_DIV_10 || (accum == MAX_LONG_DIV_10 && (digit == 9 || !isNegative && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
							cursor.miss();
						} else {
							accum = accum * 10 + digit
							cursor.advance();
							pointedElem = cursor.pointedElem;
							if ('0' <= pointedElem && pointedElem <= '9') {
								cursor.miss();
							}
						}
					}
				}
				if (isNegative) -accum
				else accum
			}
		} else {
			0L
		}
	}
}






