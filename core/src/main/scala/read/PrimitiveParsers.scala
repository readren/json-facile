package read


object PrimitiveParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;

	val jpString: Parser[String] = SyntaxParsers.string.orFail("A string was expected.");

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

	import scala.reflect.runtime.{universe => ru}

	def jpEnumeration[E <: scala.Enumeration](implicit typeTag: ru.TypeTag[E]): Parser[E#Value] = {
		val (enum, fullName) = {
			// TODO consider using a cache. When is this code executed? Compile or run time?
			val eType = typeTag.tpe;
			val moduleSymbol = eType.termSymbol.asModule
			val classLoaderMirror = ru.runtimeMirror(getClass.getClassLoader)
			val moduleMirror = classLoaderMirror.reflectModule(moduleSymbol)
			val enum = moduleMirror.instance.asInstanceOf[E]
			(enum, moduleSymbol.fullName)
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
	}
}






