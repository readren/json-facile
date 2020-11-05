package jsfacile.read

import jsfacile.read.Parser._

/** Both the methods of this object and the [[Parser]]s given by them are thread safe (or should be). */
object SyntaxParsers {

	/** skips the next integer or miss if no integer follows
	 * @return true iff an integer was consumed and the cursor is [[ok]]*/
	def skipInteger(cursor: Cursor): Boolean = {
		val isNeg = cursor.consumeChar('-')
		val hasMantisa = cursor.consumeChar('0') || cursor.consumeCharIf(Character.isDigit) && cursor.consumeWhile(Character.isDigit)
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
	/** skips the next fraction or do nothing if no fraction follows */
	private def skipOptionalFraction(cursor: Cursor): Unit = {
		if (cursor.consumeChar('.')) {
			val hasADigit = cursor.consumeCharIf(Character.isDigit) && cursor.consumeWhile(Character.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected after the decimal point")
			}
		}
	}
	/** skips the next exponent or do nothing if no exponent follows */
	private def skipOptionalExponent(cursor: Cursor): Unit = {
		if (cursor.consumeCharIf(c => c == 'e' || c == 'E')) {
			cursor.consumeCharIf(c => c == '+' || c == '-');
			val hasADigit = cursor.consumeCharIf(Character.isDigit) && cursor.consumeWhile(Character.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected as exponent")
			}
		}
	}
	/** skips the next number or miss if no number follows
	 * @return true iff a number was consumed and the cursor is [[ok]] */
	def skipJsNumber(cursor: Cursor): Boolean = {
		if(skipInteger(cursor)) {
			skipOptionalFraction(cursor);
			skipOptionalExponent(cursor);
			cursor.ok
		} else {
			cursor.miss();
			false
		}
	}

	/** skips the next "null" or miss if what follows isn't a "null"
	 * @return true iff a "null" was consumed and the cursor is [[ok]] */
	private def skipJsNull(cursor: Cursor): Boolean = cursor.comes("null") || { cursor.miss(); false }

	/** skips the next boolean of miss if no boolean follows
	 * @return true iff a boolean was consumed and the cursor is [[ok]] */
	private def skipJsBoolean(cursor: Cursor): Boolean = cursor.comes("true") || cursor.comes( "false") || { cursor.miss(); false }

	/** skips the next string or miss if no string follows
	 * @return true iff a string was consumed and the cursor is [[ok]] */
	private def skipJsString(cursor: Cursor): Boolean = { string.parse(cursor); cursor.ok }

	/** skips the next json array or miss if no array follows
	 * @return true iff an array was consumed and the cursor is [[ok]] */
	private def skipJsArray(cursor: Cursor): Boolean = {
		if(cursor.consumeChar('[')) {
			var have = cursor.consumeWhitespaces()
			while(have && cursor.pointedElem != ']') {
				skipJsValue(cursor);
				have = cursor.consumeWhitespaces() && (cursor.consumeChar(',') && cursor.consumeWhitespaces() || cursor.pointedElem == ']')
			}
			if(have) {
				cursor.advance()
			} else {
				cursor.fail("Invalid json array syntax found while skipping")
			}
		} else {
			cursor.miss();
		}
		cursor.ok
	}

	/** skips the next json object or miss if no object follows
	 * @return true iff an object was consumed and the cursor is [[ok]] */
	def skipJsObject(cursor: Cursor): Boolean = {
		if(cursor.consumeChar('{')) {
			var ok = cursor.consumeWhitespaces();
			while (ok && cursor.pointedElem != '}') {
				skipJsString(cursor)
				cursor.consumeWhitespaces()
				cursor.consumeChar(':')
				ok = cursor.consumeWhitespaces() && (cursor.consumeChar(',') && cursor.consumeWhitespaces() || cursor.pointedElem == '}')
			}
			if(ok) {
				cursor.advance()
			} else {
				cursor.fail("Invalid json object syntax found while skipping")
			}
		} else {
			cursor.miss();
		}
		cursor.ok
	}

	/** skips the next json value or miss if no json value follows
	 * @return true iff a json value was consumed and the cursor is [[ok]] */
	def skipJsValue(cursor: Cursor): Boolean = {
		if (cursor.have) {
			val p: Cursor => Boolean = cursor.pointedElem match {
				case '"' => skipJsString
				case '{' => skipJsObject
				case '[' => skipJsArray
				case 't' | 'f' => skipJsBoolean
				case 'n' => skipJsNull
				case _ => skipJsNumber
			}
			p(cursor);
		} else {
			cursor.miss();
			false
		}
	}

	/** A [[Parser]] of json strings.
	 * Used to parse the json field names and string values.
	 * Differs from [[PrimitiveParsers.jpString]] in that the second sets the failure flag when the match misses. */
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
