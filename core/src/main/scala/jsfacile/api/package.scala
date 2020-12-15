package jsfacile

package object api {
	//////////////////////
	//// Aliases ////

	type IterableUpperBound[E] = scala.collection.Iterable[E];
	type MapUpperBound[K, V] = scala.collection.Map[K, V];
	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V];

	type Parser[A] = jsfacile.read.Parser[A]
	val Parser: jsfacile.read.Parser.type = jsfacile.read.Parser;
	type Cursor = jsfacile.read.Cursor;
	type CursorStr = jsfacile.read.CursorStr;

	type Appender[A] = jsfacile.write.Appender[A];
	val Appender: jsfacile.write.Appender.type = jsfacile.write.Appender;
	type Record = jsfacile.write.Record;
	type RecordStr = jsfacile.write.RecordStr;

	type discriminatorField = jsfacile.annotations.discriminatorField;
	type DiscriminatorDecider[C] = jsfacile.joint.DiscriminatorDecider[C]
	type DiscriminatorConf = jsfacile.joint.DiscriminatorConf;

	type MapFormatDecider[K, V, MC[_,_]] = jsfacile.write.MapFormatDecider[K, V, MC];

	///////////////////
	//// Summoners ////

	/** Summons a [[Parser]] instance of the specified type */
	def parserOf[A](implicit pa: Parser[A]): Parser[A] = pa;

	/** Summons an [[Appender]] instance of the specified type */
	def appenderOf[A](implicit aoa: Appender[A]): Appender[A] = aoa;


	///////////////////////////////////
	//// Existent types enrichment ////

	/** Adds the [[toJson]] method to all objects */
	implicit class ToJsonConvertible[T](val obj: T) extends AnyVal {
		def toJson(implicit at: Appender[T]): String = {
			val r = new RecordStr(new java.lang.StringBuilder());
			at.append(r, obj);
			r.sb.toString
		}
	}

	/** Adds the [[fromJson]] method to String */
	implicit class FromJsonConvertible(val jsonDoc: String) extends AnyVal {
		/** Tries to create an instance of the specified type with the value represented by this [[String]] in JSON format.
		 * @tparam T the type of the instance to be created. This type parameter should be specified explicitly. */
		def fromJson[T](implicit pt: Parser[T]): Either[ParseError, T] = {
			val cursor = new CursorStr(jsonDoc);
			val result = pt.parse(cursor);
			if (cursor.ok) {
				if (cursor.isPointing)
					Left(ParseIncomplete(jsonDoc, cursor.pos));
				else
					Right(result)
			} else if (cursor.failed) {
				Left(ParseFailure(jsonDoc, cursor.pos, cursor.failureCause))
			} else {
				val missCause =
					if(cursor.missCause!=null) cursor.missCause
					else "The json representation is not compatible with the expected type"
				Left(ParseMiss(jsonDoc, cursor.pos, missCause));
			}
		}
	}

	trait ParseError {
		def jsonDoc: String
		def pos: Parser.Pos;
		def toString: String
		def visualizePos: String = {
			val sb = new StringBuilder(jsonDoc);
			sb.append('\n')
			var count = pos;
			while(count>0) {
				sb.append(' ')
				count -= 1;
			}
			sb.append('^')
			sb.append('\n')
			sb.result();
		}
	}
	case class ParseIncomplete(jsonDoc: String, pos: Parser.Pos) extends ParseError {
		override def toString = s"""The json input was not entirely consumed. The parsing stopped at position $pos. The remaining fragment surrounded with '>' and '<' is: >${jsonDoc.substring(pos)}<."""
	}
	case class ParseMiss(jsonDoc: String, pos: Parser.Pos, expected: String = null) extends ParseError {
		override def toString = s"The parsing missed at position $pos. ${if (expected != null) expected else ""}"
	}
	case class ParseFailure(jsonDoc: String, pos: Parser.Pos, cause: AnyRef) extends ParseError {
		override def toString = s"The parsing failed at position $pos. Cause: $cause"
	}
}
