package jsfacile.read

/** Parser of scala `object` instances */
class SingletonParser[S](instance: S) extends Parser[S] {

	override def parse(cursor: Cursor): S = {
		val ok = Skip.jsObject(cursor);
		if (!ok) {
			cursor.miss(s"A json object was expected while parsing the singleton object ${instance.getClass.getName}")
		}
		instance
	}
}
