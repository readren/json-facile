package write

import write.MapAppender.{MapFormatDecider, defaultMapFormatDecider}

/** Contains the implicit appenders that require import tax.
 * The implicit defined in this package object should be imported in order to have more precedence than [[write.jaProduct]] and [[write.jaCoproduct]], which should NOT be imported. */
package object api {
	////////////////
	//// Suggar ////

	/** Adds the [[toJson]] method to all objects */
	implicit class ToJsonConvertable[T](val obj: T) extends AnyVal {
		def toJson(implicit at: Appender[T]): String = {
			val r = new RecordStr(new java.lang.StringBuilder())
			at.append(r, obj);
			r.sb.toString
		}
	}

	//////////////////////////////////////////
	//// Supported standard library types ////


	implicit def jaEnumeration[E <: scala.Enumeration]: Appender[E#Value] = (r, enum) => r.append(enum.toString)

	@inline implicit def jaIterable[E, IC[e] <: Iterable[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] = IterableAppender.apply[E, IC](elemAppender)

	@inline implicit def jaMap[K, V, MC[k, v] <: Map[k, v]](
		implicit
		ka: Appender[K],
		va: Appender[V],
		mfd: MapFormatDecider[K, V, MC[K, V]] = defaultMapFormatDecider
	): Appender[MC[K, V]] = MapAppender.apply[K, V, MC](ka, va, mfd)


}
