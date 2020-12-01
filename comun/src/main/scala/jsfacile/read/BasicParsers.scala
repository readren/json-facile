package jsfacile.read

import jsfacile.read.Parser._

object BasicParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;
	private val THRESHOLD = 40; // length of the string chunk above which it is faster to create an append a string than to append the contained characters one by one. TODO calculate the THRESHOLD

	/** A [[Parser]] of JSON strings.
	 * Used to parse the JSON field names and string values. */
	object jpString extends Parser[String] {
		override def parse(cursor: Cursor): String = {
			if (cursor.have) {
				val nextSingularityPos = cursor.posOfNextEscapeOrClosingQuote;
				if (nextSingularityPos == 0) { // nextPos == 0 <=> cursor.pointedElem != '"'
					cursor.miss();
					ignored[String];
				} else if (cursor.isPointing) {
					if (cursor.pointedElem == '"') {
						cursor.consumeStringUntil(nextSingularityPos)
					} else {
						// reaches here if at the `nextSingularityPos` is an escape '\' char
						val distance = nextSingularityPos - cursor.pos;
						val sb = new java.lang.StringBuilder(distance + 16)
						if (distance > THRESHOLD) {
							sb.append(cursor.consumeStringTo(nextSingularityPos))
							// here the pointedElem should be an escape char '\'.
						}
						var pe = cursor.pointedElem;
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
							pe = cursor.pointedElem
						}
						cursor.advance() // consume closing quote char
						sb.toString
					}


				} else {
					cursor.fail("unclosed string")
					ignored[String]
				}
			} else {
				cursor.miss()
				ignored[String]
			}
		}
	}


	val jpUnit: Parser[Unit] = (acceptStr("null") ^^^ ()) withMissCause "A null was expected";

	val jpNull: Parser[Null] = (acceptStr("null") ^^^ null) withMissCause "A null was expected";

	object jpBoolean extends Parser[Boolean] {
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


	/** Interpretador de Int en Json */
	object jpInt extends Parser[Int] {
		override def parse(cursor: Cursor): Int = {
			var have = cursor.have
			if (have) {
				cursor.attempt { () =>
					var accum: Int = 0;
					var limit = 9; // mantissa length
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
							cursor.fail("A digit was expected after the minus sign when parsing a Int")
						} else {
							cursor.miss("The first digit of an Int was expected")
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
							if (accum > MAX_INT_DIV_10 || (accum == MAX_INT_DIV_10 && (digit == 9 || !isNegative && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
								cursor.fail("Overflow: The number being parsed can't be represented by a `scala.Int`.");
							} else {
								accum = accum * 10 + digit
								have = cursor.advance();
								if (have) {
									pointedElem = cursor.pointedElem;
									if ('0' <= pointedElem && pointedElem <= '9') {
										cursor.fail("Overflow: The number being parsed can't be represented by a `scala.Int`.");
									}
								}
							}
						}
					}
					if (isNegative) -accum
					else accum
				}
			} else {
				cursor.miss("An Int was expected")
				0
			}
		}
	}

	/** Interpretador de Long en Json */
	object jpLong extends Parser[Long] {
		override def parse(cursor: Cursor): Long = {
			var have = cursor.have;
			if (have) {
				cursor.attempt { () =>
					var accum: Long = 0;
					var limit = 18; // mantissa length

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
							if (accum > MAX_LONG_DIV_10 || (accum == MAX_LONG_DIV_10 && (digit == 9 || !isNegative && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
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

	object jpBigInt extends Parser[BigInt] {
		override def parse(cursor: Cursor): BigInt = {
			val integer = cursor.stringConsumedBy(Skip.integer)
			if (cursor.ok && integer.length > 0) {
				BigInt(integer);
			} else {
				cursor.miss("An integer number was expected while parsing a BigInt");
				Parser.ignored[BigInt]
			}
		}
	}

	object jpBigDecimal extends Parser[BigDecimal] {
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

	object jpDouble extends Parser[Double] {
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

	object jpFloat extends Parser[Float] {
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

	def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = { cursor =>
		if (cursor.comes("null")) {
			None
		} else {
			Some(pE.parse(cursor))
		}
	}

	def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = { cursor => Some(pE.parse(cursor)) }

	object jpNone extends Parser[None.type] {
		override def parse(cursor: Cursor): None.type = {
			if (!cursor.comes("null")) {
				cursor.miss("A null was expected while trying to parse a None")
			}
			None
		}
	}

	/** The right parser has priority. If it hits the left parser is ignored.
	 * Implemented with an optimized versión of {{{(pR ^^ { r => Right[L, R](r).asInstanceOf[Either[L, R]] }) | (pL ^^ { l => Left[L, R](l).asInstanceOf[Either[L, R]] })}}} */
	def jpEither[L, R](implicit pL: Parser[L], pR: Parser[R]): Parser[Either[L, R]] = { cursor =>
		val r = cursor.attempt(() => pR.parse(cursor));
		if (cursor.ok) {
			Right(r)
		} else {
			cursor.repair();
			Left(pL.parse(cursor))
		}
	}

	def jpLeft[L, R](implicit pL: Parser[L]): Parser[Left[L, R]] = pL ^^ Left[L, R]
	def jpRight[L, R](implicit pR: Parser[R]): Parser[Right[L, R]] = pR ^^ Right[L, R]

}
