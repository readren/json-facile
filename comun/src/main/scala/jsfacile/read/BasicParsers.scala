package jsfacile.read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

import jsfacile.read.Parser._
import jsfacile.util.BinarySearch

object BasicParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;

	/** A [[Parser]] of JSON strings.
	 * Used to parse the JSON field names and string values. */
	object jpString extends Parser[String] {
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


	val jpUnit: Parser[Unit] = (acceptStr("null") ^^^ ()) withMissCause "A null was expected";

	val jpNull: Parser[Null] = (acceptStr("null") ^^^ null) withMissCause "A null was expected";

	val jpBoolean: Parser[Boolean] = { cursor =>
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
	val jpInt: Parser[Int] = { cursor =>
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
					if(isNegative) {
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

	/** Interpretador de Long en Json */
	val jpLong: Parser[Long] = { cursor =>
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
					if(isNegative) {
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

	val jpBigInt: Parser[BigInt] = { cursor =>
		val integer = cursor.stringConsumedBy(Skip.integer)
		if (cursor.ok && integer.length > 0) {
			BigInt(integer);
		} else {
			cursor.miss("An integer number was expected while parsing a BigInt");
			Parser.ignored[BigInt]
		}
	}

	val jpBigDecimal: Parser[BigDecimal] = { cursor =>
		val number = cursor.stringConsumedBy(Skip.jsNumber)
		if (cursor.ok) {
			BigDecimal(number);
		} else {
			cursor.miss("A number was expected while parsing a BigDecimal");
			Parser.ignored[BigDecimal]
		}
	}

	val jpDouble: Parser[Double] = { cursor =>
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

	val jpFloat: Parser[Float] = { cursor =>
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

	def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = { cursor =>
		if (cursor.comes("null")) {
			None
		} else {
			Some(pE.parse(cursor))
		}
	}

	def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = { cursor => Some(pE.parse(cursor)) }

	val jpNone: Parser[None.type] = { cursor =>
		if (!cursor.comes("null")) {
			cursor.miss("A null was expected while trying to parse a None")
		}
		None
	}

	/** The right parser has priority. If it hits the left parser is ignored.
	 *  Implemented with an optimized versión of {{{(pR ^^ { r => Right[L, R](r).asInstanceOf[Either[L, R]] }) | (pL ^^ { l => Left[L, R](l).asInstanceOf[Either[L, R]] })}}} */
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
