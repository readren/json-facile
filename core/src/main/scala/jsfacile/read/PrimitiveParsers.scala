package jsfacile.read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

import jsfacile.read.Parser._
import jsfacile.util.BinarySearch

object PrimitiveParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;

	val jpString: Parser[String] = SyntaxParsers.string;

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
		val integer = cursor.stringConsumedBy(SyntaxParsers.skipInteger)
		if (cursor.ok && integer.length > 0) {
			BigInt(integer);
		} else {
			cursor.miss("An integer number was expected while parsing a BigInt");
			Parser.ignored[BigInt]
		}
	}

	val jpBigDecimal: Parser[BigDecimal] = { cursor =>
		val number = cursor.stringConsumedBy(SyntaxParsers.skipJsNumber)
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
			val number = cursor.stringConsumedBy(SyntaxParsers.skipJsNumber)
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
			val number = cursor.stringConsumedBy(SyntaxParsers.skipJsNumber)
			if (cursor.ok) {
				java.lang.Float.parseFloat(number);
			} else {
				cursor.miss("A number was expected while parsing a Float");
				Parser.ignored[Float]
			}
		}
	}


	private val jpEnumerationCache = mutable.WeakHashMap.empty[String, Parser[_ <: Enumeration#Value]]
	import scala.reflect.runtime.{universe => ru}

	def jpEnumeration[E <: scala.Enumeration](implicit typeTag: ru.TypeTag[E]): Parser[E#Value] = {
		val fullName = typeTag.tpe.toString
		jpEnumerationCache.getOrElseUpdate(
		fullName, {
			val enum = { // TODO use a macro to obtain this to avoid the mirror
				val classLoaderMirror = ru.runtimeMirror(getClass.getClassLoader)
				val moduleMirror = classLoaderMirror.reflectModule(typeTag.tpe.termSymbol.asModule)
				moduleMirror.instance.asInstanceOf[E]
			}
			val values = ArraySeq.from(enum.values)

			{ cursor =>
				if (cursor.have) {
					if (cursor.pointedElem == '"') {
						val name = SyntaxParsers.string.parse(cursor);
						if (cursor.ok) {
							val index = values.indexWhere(_.toString == name)
							if (index >= 0) {
								values(index);
							} else {
								cursor.miss(s"""The expected enum "$fullName" does not contain a value with this name: $name.""")
								ignored[E#Value]
							}
						} else {
							ignored[E#Value]
						}
					} else {
						val id = jpInt.parse(cursor)
						if (cursor.ok) {
							val value = BinarySearch.find(values.unsafeArray.asInstanceOf[Array[enum.Value]])(_.id - id)
							if (value == null) {
								cursor.miss(s"""The expected enum "$fullName" does not contain a value with this id: $id.""")
							}
							value
						} else {
							cursor.miss(s"""A string with the name or an integer with the id of an element of the enum "$fullName" was expected.""")
							ignored[E#Value]
						}
					}

				} else {
					cursor.miss(s"""A value of the enum "$fullName" was expected but the end of the content was reached.""")
					null
				}
			}
		}).asInstanceOf[Parser[E#Value]]
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

	/** Implemented with an optimized versión of {{{(pR ^^ { r => Right[L, R](r).asInstanceOf[Either[L, R]] }) | (pL ^^ { l => Left[L, R](l).asInstanceOf[Either[L, R]] })}}} */
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
