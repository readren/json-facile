package jsfacile.read

import jsfacile.read.Parser._

object BasicParsers extends BasicParsers {

	private val MAX_BYTE_DIV_10 = java.lang.Byte.MAX_VALUE / 10;
	private val MAX_BYTE_DIGITS = MAX_BYTE_DIV_10.toString.length;
	private val MAX_SHORT_DIV_10 = java.lang.Short.MAX_VALUE / 10;
	private val MAX_SHORT_DIGITS = MAX_SHORT_DIV_10.toString.length;

	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;
	private val MAX_INT_DIGITS = MAX_INT_DIV_10.toString.length;
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_LONG_DIGITS = MAX_LONG_DIV_10.toString.length;

}

trait BasicParsers {
	import BasicParsers._

	/** A [[Parser]] of JSON strings.
	 * Used to parse the JSON field names and string values. */
	implicit object jpString extends Parser[String] {
		override def parse(cursor: Cursor): String = {
			if (cursor.isPointing) {
				val nextSingularityPos = cursor.posOfNextEscapeOrClosingQuote;
				if (nextSingularityPos == 0) { // nextPos == 0 <=> cursor.pointedElem != '"'
					cursor.miss(); // because string must start with a quote.
					ignored[String];
				} else {
					val pf = cursor.consumeStringTo(nextSingularityPos)
					if (!cursor.isPointing) {
						cursor.fail("unclosed string")
						ignored[String]
					} else {
						var pe = cursor.pointedElem;
						if (pe == '"') {
							cursor.advance(); // consume closing quote char
							pf
						} else {
							// reaches here when the pointed element is an escape char '\'
							val sb = new java.lang.StringBuilder(pf)
							while (pe != '"') {
								if (pe == '\\') {
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
								} else {
									sb.append(pe)
								}

								if (!cursor.advance()) {
									cursor.fail("unclosed string");
									return ignored[String]
								}
								pe = cursor.pointedElem;
							}
							cursor.advance() // consume closing quote char
							sb.toString
						}
					}
				}
			} else {
				cursor.miss()
				ignored[String]
			}
		}
	}

	implicit val jpCharSequence: Parser[CharSequence] = jpString.asInstanceOf[Parser[CharSequence]]

	implicit val jpUnit: Parser[Unit] = (acceptStr("null") ^^^ ()) withMissCause "A null was expected";

	implicit val jpNull: Parser[Null] = (acceptStr("null") ^^^ null) withMissCause "A null was expected";

	implicit object jpBoolean extends Parser[Boolean] {
		override def parse(cursor: Cursor): Boolean =
			if (cursor.comes("true")) {
				true
			} else if (cursor.comes("false")) {
				false
			} else {
				cursor.miss("A boolean was expected")
				false
			}
	}

	implicit object jpByte extends Parser[Byte] {
		override def parse(cursor: Cursor): Byte = {
			if (cursor.isPointing) {
				cursor.attempt(jpInteger("Byte", MAX_BYTE_DIGITS, MAX_BYTE_DIV_10)).toByte
			} else {
				cursor.miss("A Byte was expected")
				0
			}
		}
	}

	implicit object jpShort extends Parser[Short] {
		override def parse(cursor: Cursor): Short = {
			if (cursor.isPointing) {
				cursor.attempt {jpInteger("Short", MAX_SHORT_DIGITS, MAX_SHORT_DIV_10)}.toShort
			} else {
				cursor.miss("A Short was expected")
				0
			}
		}
	}

	implicit object jpInt extends Parser[Int] {
		override def parse(cursor: Cursor): Int = {
			if (cursor.isPointing) {
				cursor.attempt(jpInteger("Int", MAX_INT_DIGITS, MAX_INT_DIV_10))
			} else {
				cursor.miss("An Int was expected")
				0
			}
		}
	}


	private final def jpInteger(name: String, maxWholeDigits: Int, maxValueDiv10: Int)(cursor: Cursor): Int = {
		var have = true;
		var accum: Int = 0;
		var limit = maxWholeDigits; // mantissa length
		var pointedElem = cursor.pointedElem;
		val isNegative = pointedElem == '-';
		if (isNegative) {
			have = cursor.advance();
			if (have) {
				pointedElem = cursor.pointedElem
			}
		};
		var digit = pointedElem - '0';
		if (digit < 0 || 9 < digit) {
			if (isNegative) {
				cursor.fail(s"A digit was expected after the minus sign when parsing a $name")
			} else {
				cursor.miss("The first digit of an integer number was expected")
			}
		} else {
			do {
				accum = accum * 10 + digit;
				limit -= 1;
				have = cursor.advance();
				if (have) {
					pointedElem = cursor.pointedElem;
					digit = pointedElem - '0';
				}
			} while (0 <= digit && digit <= 9 && have && limit > 0);

			if (0 <= digit && digit <= 9 && have) {
				if (accum > maxValueDiv10 || (accum == maxValueDiv10 && (digit == 9 || !isNegative && digit == 8))) { // Note that for all primitive integers MaxValue/10 == -MinValue/10 and the least significant digit of MaxValue is 7 and of MinValue is 8. Otherwise there would have to deal with the differences
					cursor.fail(s"Overflow: The number being parsed can't be represented by a `scala.$name`.");
				} else {
					accum = accum * 10 + digit
					have = cursor.advance();
					if (have) {
						pointedElem = cursor.pointedElem;
						if ('0' <= pointedElem && pointedElem <= '9') {
							cursor.fail(s"Overflow: The number being parsed can't be represented by a `scala.$name`.");
						}
					}
				}
			}
		}
		if (isNegative) -accum
		else accum
	}


	implicit object jpLong extends Parser[Long] {
		override def parse(cursor: Cursor): Long = {
			var have = cursor.have;
			if (have) {
				cursor.attempt { cursor =>
					var accum: Long = 0;
					var limit = MAX_LONG_DIGITS;

					var pointedElem = cursor.pointedElem;
					val isNegative = pointedElem == '-';
					if (isNegative) {
						have = cursor.advance();
						if (have) {
							pointedElem = cursor.pointedElem
						}
					};
					var digit = pointedElem - '0';
					if (digit < 0 || 9 < digit) {
						if (isNegative) {
							cursor.fail("A digit was expected after the minus sign when parsing a Long")
						} else {
							cursor.miss("The first digit of a Long was expected")
						}

					} else {
						do {
							accum = accum * 10L + digit;
							limit -= 1;
							have = cursor.advance();
							if (have) {
								pointedElem = cursor.pointedElem;
								digit = pointedElem - '0';
							}
						} while (0 <= digit && digit <= 9 && have && limit > 0);

						if (0 <= digit && digit <= 9 && have) {
							if (accum > MAX_LONG_DIV_10 || (accum == MAX_LONG_DIV_10 && (digit == 9 || !isNegative && digit == 8))) { // Note that MAX_LONG/10 == MIN_LONG/10. Otherwise there would have to deal with the difference
								cursor.fail("Overflow: The number being parsed can't be represented by a `scala.Long`.");
							} else {
								accum = accum * 10 + digit
								have = cursor.advance();
								if (have) {
									pointedElem = cursor.pointedElem;
									if ('0' <= pointedElem && pointedElem <= '9') {
										cursor.fail("Overflow: The number being parsed can't be represented by a `scala.Long`.");
									}
								}
							}
						}
					}
					if (isNegative) -accum
					else accum
				}
			} else {
				cursor.miss("A Long was expected")
				0L
			}
		}
	}

	implicit object jpBigInt extends Parser[BigInt] {
		override def parse(cursor: Cursor): BigInt = {
			val integer = cursor.stringConsumedBy(Skip.integer)
			if (cursor.ok && integer.nonEmpty) {
				BigInt(integer);
			} else {
				cursor.miss("An integer number was expected while parsing a BigInt");
				Parser.ignored[BigInt]
			}
		}
	}

	implicit object jpBigDecimal extends Parser[BigDecimal] {
		override def parse(cursor: Cursor): BigDecimal = {
			val number = cursor.stringConsumedBy(Skip.jsNumber)
			if (cursor.ok) {
				BigDecimal.exact(number);
			} else {
				cursor.miss("A number was expected while parsing a BigDecimal");
				Parser.ignored[BigDecimal]
			}
		}
	}

	implicit object jpDouble extends Parser[Double] {
		override def parse(cursor: Cursor): Double = {
			if (cursor.comes("null")) {
				Double.NaN
			} else {
				val number = cursor.stringConsumedBy(Skip.jsNumber)
				if (cursor.ok) {
					java.lang.Double.parseDouble(number);
				} else {
					cursor.miss("A number was expected while parsing a Double");
					Parser.ignored[Double]
				}
			}
		}
	}

	implicit object jpFloat extends Parser[Float] {
		override def parse(cursor: Cursor): Float = {
			if (cursor.comes("null")) {
				Float.NaN
			} else {
				val number = cursor.stringConsumedBy(Skip.jsNumber)
				if (cursor.ok) {
					java.lang.Float.parseFloat(number);
				} else {
					cursor.miss("A number was expected while parsing a Float");
					Parser.ignored[Float]
				}
			}
		}
	}

	implicit def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = { cursor =>
		if (cursor.comes("null")) {
			None
		} else {
			Some(pE.parse(cursor))
		}
	}

	implicit def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = { cursor => Some(pE.parse(cursor)) }

	implicit object jpNone extends Parser[None.type] {
		override def parse(cursor: Cursor): None.type = {
			if (!cursor.comes("null")) {
				cursor.miss("A null was expected while trying to parse a None")
			}
			None
		}
	}

	/** The right parser has priority. If it hits the left parser is ignored.
	 * Implemented with an optimized versión of {{{(pR ^^ { r => Right[L, R](r).asInstanceOf[Either[L, R]] }) | (pL ^^ { l => Left[L, R](l).asInstanceOf[Either[L, R]] })}}} */
	implicit def jpEither[L, R](implicit pL: Parser[L], pR: Parser[R]): Parser[Either[L, R]] = { cursor =>
		val r = cursor.attempt(pR.parse);
		if (cursor.ok) {
			Right(r)
		} else {
			cursor.repair();
			Left(pL.parse(cursor))
		}
	}

	implicit def jpLeft[L, R](implicit pL: Parser[L]): Parser[Left[L, R]] = pL ^^ Left[L, R]
	implicit def jpRight[L, R](implicit pR: Parser[R]): Parser[Right[L, R]] = pR ^^ Right[L, R]

}
