import read.CoproductParserHelper.Coproduct

/** The implicit defined in this package object should NOT be imported in order to have less precedence than the implicit defined in the [[write.api]] package object, which should be imported.
 * The compiler finds the implicit defined here when it searches for instances of the [[Appender]] trait because it belongs to this package object. */
package object write {

	////////////////////////////////////////////
	//// Json appenders for primitive types ////

	implicit val jaNull: Appender[Null] = { (r: Record, a: Null) => r.append("null") }

	implicit val jaBoolean: Appender[Boolean] = { (r, bool) => r.append(if (bool) "true" else "false") }

	implicit val jaInt: Appender[Int] = { (r, int) => r.append(int) }

	implicit val jaLong: Appender[Long] = { (r, long) => r.append(long) }

	implicit val jaFloat: Appender[Float] = { (r, float) =>
		if (float.isNaN) r.append("null")
		else r.append(float)
	}

	implicit val jaDouble: Appender[Double] = { (r, double) =>
		if (double.isNaN) r.append("null")
		else r.append(double)
	}

	implicit val jaChar: Appender[Char] = { (r, char) =>
		r.append('"');
		encodeStringChar(r, char).append('"')
	}

	///////////////////////////////////////////
	//// Json appenders for standard types ////

	implicit val jaCharSequence: Appender[CharSequence] = { (r, csq) =>
		r.append('"');
		encodeStringCharSequence(r, csq).append('"')
	}

	implicit val jaString: Appender[String] = jaCharSequence.asInstanceOf[Appender[String]]

	implicit val jaBigInt: Appender[BigInt] = { (r, bigInt) => r.append(bigInt.toString()) }

	implicit val jaBigDecimal: Appender[BigDecimal] = { (r, bigDec) => r.append(bigDec.toString()) }

	////////////////////////////////////////////////////////////////////////////
	//// Json appenders for products and coproducts (sealed abstract types) ////

	implicit def jaProduct[P <: ProductAppender.UpperBound]: Appender[P] = macro ProductAppender.materializeImpl[P]

	implicit def jaCoproduct[C <: Coproduct](implicit helper: CoproductAppenderHelper[C]): Appender[C] = new CoproductAppender(helper);

	//////////////////////////////
	//// Json string enconders ////

	/** Encodes the received [[Char]] in order to be part of a json string
	 * Surrogate chars are not altered. */
	def encodeStringChar(r: Record, char: Char): Record = {
		if (char == '"') {
			r.append('\\').append(char)
		} else if (char == '\\') {
			r.append('\\').append('\\')
		} else if (char < 32) {
			r.append('\\');
			char match {
				case '\t' => r.append('t')
				case '\n' => r.append('n')
				case '\r' => r.append('r')
				case '\f' => r.append('f')
				case '\b' => r.append('b')
				case ctrl =>
					r.append("u00")
					if (ctrl < 16)
						r.append('0')
					r.append(Integer.toHexString(ctrl))
			}
			r
		} else {
			r.append(char)
		}
	}

	/** Encodes the received [[CharSequence]] in order to be part of a json string */
	def encodeStringCharSequence(r: Record, csq: CharSequence): Record = {
		var index = 0;
		val length = csq.length;
		while (index < length) {
			// Note that it is not necessary to treat surrogate chars differently because the encodeStringChar method don't alters them.
			encodeStringChar(r, csq.charAt(index));
			index += 1;
		}
		r
	}
}
