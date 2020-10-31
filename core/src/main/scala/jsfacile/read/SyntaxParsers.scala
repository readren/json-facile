package jsfacile.read

import scala.collection.mutable

import jsfacile.read.Parser._

/** Both the methods of this object and the [[Parser]]s given by them are thread safe (or should be). */
object SyntaxParsers {

	type CodePoint = Int;

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
		var have = cursor.have;
		while (have && Character.isWhitespace(cursor.pointedElem)) {
			have = cursor.advance()
		}
		cursor.pos
	}
	val colon: Parser[Elem] = ':'
	val coma: Parser[Elem] = ','

	private val digit: Parser[Elem] = acceptElemIf(Character.isDigit)
	private val skipDigits: Parser[Pos] = { cursor =>
		cursor.consumeWhile(_.isDigit)
		cursor.pos
	}
	private val digit19: Parser[Elem] = acceptElemIf(e => '1' <= e && e <= '9')

	//////////////////

	def skipInteger(cursor: Cursor): Boolean = {
		// Optimized version of: '-'.opt ~> ('0' ~> pos) | (digit19 ~> skipDigits)
		val isNeg = cursor.consumeChar('-')
		val hasMantisa = cursor.consumeChar('0') || cursor.consumeCharIf(_.isDigit) && cursor.consumeWhile(_.isDigit)
		if (hasMantisa) {
			cursor.ok
		} else {
			if (isNeg) {
				cursor.fail("A digit was expected")
			} else {
				cursor.miss()
			}
			false
		}
	}
	private def skipOptionalFraction(cursor: Cursor): Unit = {
		// optimized version of: '.' ~> digit ~> skipDigits
		if (cursor.consumeChar('.')) {
			val hasADigit = cursor.consumeCharIf(_.isDigit) && cursor.consumeWhile(_.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected after the decimal point")
			}
		}
	}
	private def skipOptionalExponent(cursor: Cursor): Unit = {
		// optimized version of:  acceptElemIf(elem => elem == 'e' || elem == 'E') ~> acceptElemIf(elem => elem == '+' || elem == '-').opt ~> digit ~> skipDigits // me parece que en lugar de "digit" debería ser "digit19", pero me regí por la página https://www.json.org/json-en.html
		if (cursor.consumeCharIf(c => c == 'e' || c == 'E')) {
			cursor.consumeCharIf(c => c == '+' || c == '-');
			val hasADigit = cursor.consumeCharIf(_.isDigit) && cursor.consumeWhile(_.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected as exponent")
			}
		}
	}
	val skipJsNumber: Parser[Pos] = { cursor =>
		// optimized version of: skipInteger ~> skipFraction.opt ~> skipExponent.opt ~> pos
		if(skipInteger(cursor)) {
			skipOptionalFraction(cursor);
			skipOptionalExponent(cursor)
		}
		cursor.pos;
	}

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
						if (skipObject == null) {
							skipObject = skipJsObject
						}
						skipObject
					case '[' =>
						if (skipArray == null) {
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

	object string extends Parser[String] {
		override def parse(cursor: Cursor): String = {
			if (cursor.have && cursor.pointedElem == '"') {
				if (!cursor.advance()) {
					cursor.fail("unclosed string");
					return ignored[String]
				}
				val sb = new java.lang.StringBuilder();

				var pe = cursor.pointedElem;
				while (pe != '"') {
					pe match {
						case '\\' =>
							if (!cursor.advance()) {
								cursor.fail("unfinished string escape");
								return ignored[String]
							}
							cursor.pointedElem match {
								case '"' => sb.append('"')
								case '\\' => sb.append('\\')
								case '/' => sb.append('/')
								case 'b' => sb.append('\b')
								case 'f' => sb.append('\f')
								case 'n' => sb.append('\n')
								case 'r' => sb.append('\r')
								case 't' => sb.append('\t')
								case 'u' =>
									var counter = 4;
									var hexCode: Int = 0;
									do {
										if (!cursor.advance()) {
											cursor.fail("unfinished string hex code escape");
											return ignored[String]
										}
										hexCode *= 16;
										pe = cursor.pointedElem;
										if ('0' <= pe & pe <= '9') {
											hexCode += pe - '0'
										} else if ('a' <= pe && pe <= 'f') {
											hexCode += pe - 'a' + 10
										} else if ('A' <= pe && pe <= 'F') {
											hexCode += pe - 'A' + 10
										} else {
											cursor.fail("invalid hex code");
											return ignored[String]
										}
										counter -= 1;
									} while (counter > 0)
									sb.appendCodePoint(hexCode)
							}

						case highSurrogate if highSurrogate >= Character.MIN_HIGH_SURROGATE =>
							if (!cursor.advance()) {
								cursor.fail("unfinished string in middle of surrogate pair");
								return ignored[String]
							}
							pe = cursor.pointedElem;
							if (!pe.isLowSurrogate) {
								cursor.fail("invalid surrogate pair");
								return ignored[String]
							}
							sb.append(highSurrogate).append(pe)

						case normal => sb.append(normal)
					};
					if (!cursor.advance()) {
						cursor.fail("unclosed string");
						return ignored[String]
					}
					pe = cursor.pointedElem
				}
				cursor.advance() // consume closing quote char
				sb.toString

			} else {
				cursor.miss()
				ignored[String]
			}
		}
	}
}
