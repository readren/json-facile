package jsfacile.read

/** Encloses [[Cursor]] content consumers for JSON documents elements. */
object Skip {

	/** skips the next integer or miss if no integer follows
	 * @return true iff an integer was consumed and the cursor is [[Cursor.ok]]*/
	def integer(cursor: Cursor): Boolean = {
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
	private def optionalFraction(cursor: Cursor): Unit = {
		if (cursor.consumeChar('.')) {
			val hasADigit = cursor.consumeCharIf(Character.isDigit) && cursor.consumeWhile(Character.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected after the decimal point")
			}
		}
	}
	/** skips the next exponent or do nothing if no exponent follows */
	private def optionalExponent(cursor: Cursor): Unit = {
		if (cursor.consumeCharIf(c => c == 'e' || c == 'E')) {
			cursor.consumeCharIf(c => c == '+' || c == '-');
			val hasADigit = cursor.consumeCharIf(Character.isDigit) && cursor.consumeWhile(Character.isDigit);
			if (!hasADigit) {
				cursor.fail("A digit was expected as exponent")
			}
		}
	}
	/** skips the next number or miss if no number follows
	 * @return true iff a number was consumed and the cursor is [[Cursor.ok]] */
	def jsNumber(cursor: Cursor): Boolean = {
		if(integer(cursor)) {
			optionalFraction(cursor);
			optionalExponent(cursor);
			cursor.ok
		} else {
			cursor.miss();
			false
		}
	}

	/** skips the next "null" or miss if what follows isn't a "null"
	 * @return true iff a "null" was consumed and the cursor is [[Cursor.ok]] */
	private def jsNull(cursor: Cursor): Boolean = cursor.comes("null") || { cursor.miss(); false }

	/** skips the next boolean of miss if no boolean follows
	 * @return true iff a boolean was consumed and the cursor is [[Cursor.ok]] */
	private def jsBoolean(cursor: Cursor): Boolean = cursor.comes("true") || cursor.comes( "false") || { cursor.miss(); false }

	/** skips the next string or miss if no string follows
	 * @return true iff a string was consumed and the cursor is [[Cursor.ok]] */
	private def jsString(cursor: Cursor): Boolean = { BasicParsers.jpString.parse(cursor); cursor.ok }

	/** skips the next json array or miss if no array follows
	 * @return true iff an array was consumed and the cursor is [[Cursor.ok]] */
	private def jsArray(cursor: Cursor): Boolean = {
		if(cursor.consumeChar('[')) {
			var have = cursor.consumeWhitespaces()
			while(have && cursor.pointedElem != ']') {
				jsValue(cursor);
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
	 * @return true iff an object was consumed and the cursor is [[Cursor.ok]] */
	def jsObject(cursor: Cursor): Boolean = {
		if(cursor.consumeChar('{')) {
			var ok = cursor.consumeWhitespaces();
			while (ok && cursor.pointedElem != '}') {
				jsString(cursor)
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
	 * @return true iff a json value was consumed and the cursor is [[Cursor.ok]] */
	def jsValue(cursor: Cursor): Boolean = {
		if (cursor.have) {
			val p: Cursor => Boolean = cursor.pointedElem match {
				case '"' => jsString
				case '{' => jsObject
				case '[' => jsArray
				case 't' | 'f' => jsBoolean
				case 'n' => jsNull
				case _ => jsNumber
			}
			p(cursor);
		} else {
			cursor.miss();
			false
		}
	}

}
