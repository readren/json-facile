package object read {

	////////////////
	//// Suggar ////

	/** Adds the [[toJson]] method to all objects */
	implicit class FromJsonConvertable(val string: String) extends AnyVal {
		def fromJson[T](implicit pt: Parser[T]): T =
			pt.parse(new CursorStr(string))
	}

}
