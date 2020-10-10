package read

object PrimitiveParsers {
	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;
}

trait PrimitiveParsers {
	import PrimitiveParsers._

	implicit val jpString: Parser[String] = SyntaxParsers.string;

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






