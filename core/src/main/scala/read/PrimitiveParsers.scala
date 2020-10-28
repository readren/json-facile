package read

import scala.collection.mutable;
import read.Parser._

object PrimitiveParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;

	val jpString: Parser[String] = SyntaxParsers.string.orFail("A string was expected.");

	val jpBoolean: Parser[Boolean] = { cursor =>
		if (cursor.comes("true")) {
			cursor.advance(4);
			true
		}
		else if (cursor.comes("false")) {
			cursor.advance(5)
			false
		}
		else {
			cursor.fail("A boolean was expected")
			false
		}
	}

	val jpNull: Parser[Null] = acceptStr("null") ^^^ null

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
						// Note that `cursor.fail("A digit was expected")` will be called later because the "digit" var won't contain a digit
					}
				};
				var digit = pointedElem - '0';
				if (digit < 0 || 9 < digit) {
					cursor.fail("A digit was expected");
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
			cursor.fail("An Int was expected")
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
						// Note that `cursor.fail("A digit was expected")` will be called later because the "digit" var won't contain a digit
					}
				};
				var digit = pointedElem - '0';
				if (digit < 0 || 9 < digit) {
					cursor.fail("A digit was expected");
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
			cursor.fail("A Long was expected")
			0L
		}
	}

	val jpBigInt: Parser[BigInt] = { cursor =>
		val integer = cursor.consume(() => SyntaxParsers.skipInteger.parse(cursor))
		if (cursor.ok && integer.length > 0) {
			BigInt(integer);
		} else {
			cursor.fail("An integer number was expected");
			Parser.ignored[BigInt]
		}
	}

	val jpBigDecimal: Parser[BigDecimal] = { cursor =>
		val number = cursor.consume(() => SyntaxParsers.skipJsNumber.parse(cursor))
		if (cursor.ok && number.length > 0) {
			BigDecimal(number);
		} else {
			cursor.fail("A number was expected");
			Parser.ignored[BigDecimal]
		}
	}

	val jpDouble: Parser[Double] =
		Parser.acceptStr("null").^^^(Double.NaN) | jpBigDecimal.map(_.doubleValue)

	val jpFloat: Parser[Float] =
		Parser.acceptStr("null").^^^(Float.NaN) | jpBigDecimal.map(_.floatValue)


	private val jpEnumerationCache = mutable.WeakHashMap.empty[String, Parser[_ <: Enumeration#Value]]
	import scala.reflect.runtime.{universe => ru}

	def jpEnumeration[E <: scala.Enumeration](implicit typeTag: ru.TypeTag[E]): Parser[E#Value] = {
		val fullName = typeTag.tpe.termSymbol.fullName
		jpEnumerationCache.getOrElseUpdate(
		fullName, {
			val enum = {
				val classLoaderMirror = ru.runtimeMirror(getClass.getClassLoader)
				val moduleMirror = classLoaderMirror.reflectModule(typeTag.tpe.termSymbol.asModule)
				moduleMirror.instance.asInstanceOf[E]
			}

			val asName: Parser[E#Value] = SyntaxParsers.string >> { name =>
				enum.values.find(_.toString == name) match {
					case Some(value) => Parser.hit(value)
					case None => Parser.fail(s"""The expected enum "$fullName" does not contain a value with this name: $name.""")
				}
			}
			val asId: Parser[E#Value] = jpInt >> { id =>
				enum.values.find(_.id == id) match {
					case Some(value) => Parser.hit(value)
					case None => Parser.fail(s"""The expected enum "$fullName" does not contain a value with this id: $id.""")
				}
			}
			(asName | asId).orFail(s"""A string with the name or an integer with the id of an element of the enum "$fullName" was expected.""")

		}).asInstanceOf[Parser[E#Value]]
	}

	def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = { cursor =>
		if (cursor.comes("null")) {
			None
		} else {
			Some(pE.parse(cursor))
		}
	}
	def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = pE ^^ {Some(_)}
	val jpNone: Parser[None.type] = "null" ^^^ None
}






