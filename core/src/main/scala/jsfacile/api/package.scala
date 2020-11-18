package jsfacile

package object api {
	//////////////////////
	//// Aliases ////

	type IterableUpperBound[E] = scala.collection.Iterable[E];
	type MapUpperBound[K, V] = scala.collection.Map[K, V];
	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V];


	type Parser[A] = jsfacile.read.Parser[A]
	val Parser = jsfacile.read.Parser;
	type Cursor = jsfacile.read.Parser.Cursor;
	type CursorStr = jsfacile.read.CursorStr;


	type Appender[A] = jsfacile.write.Appender[A];
	val Appender = jsfacile.write.Appender;
	type Record = jsfacile.write.Record;
	type RecordStr = jsfacile.write.RecordStr;

	///////////////////
	//// Summoners ////

	/** Summons a [[Parser]] instance of the specified type */
	def parserOf[A](implicit pa: Parser[A]): Parser[A] = pa;

	/** Summons an [[Appender]] instance of the specified type */
	def appenderOf[A](implicit aoa: Appender[A]): Appender[A] = aoa;


	///////////////////////////////////
	//// Existent types enrichment ////

	/** Adds the [[fromJson]] method to String */
	implicit class FromJsonConvertable(val string: String) extends AnyVal {
		def fromJson[T](implicit pt: Parser[T]): Either[AnyRef, T] = {
			val cursor = new CursorStr(string);
			val result = pt.parse(cursor);
			if (cursor.ok) {
				if (cursor.isPointing)
					Left(s"""The json input was not entirely consumed. The remaining fragment is: "${string.substring(cursor.pos)}".""")
				else
					Right(result)
			} else if (cursor.failed) {
				Left(s"""The parsing failed at position ${cursor.pos} with the message: ${cursor.failureCause}""")
			} else {
				Left("The json representation is not compatible with the expected type")
			}
		}
	}

	/** Adds the [[toJson]] method to all objects */
	implicit class ToJsonConvertable[T](val obj: T) extends AnyVal {
		def toJson(implicit at: Appender[T]): String = {
			val r = new RecordStr(new java.lang.StringBuilder());
			at.append(r, obj);
			r.sb.toString
		}
	}
}
