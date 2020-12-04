package jsfacile.write

import java.lang

import scala.reflect.runtime.{universe => ru}

import jsfacile.joint.MapUpperBound

object MapAppender {

	/** Put an instance of this trait into implicit scope to determine the JSON format of scala map collections: JSON object o JSON array.
	 * It was decided to make [[MapFormatDecider]] be contravariant on `V` and `MC` type parameters to allow providing an implicit value with a `val` instead of a `def` when the map value's type or collection type constructor are irrelevant, which uses to be.  For example: {{{implicit val mfd: MapFormatDecider[Foo, Any, SortedMap] = ??? }}} would determine the map format for all maps that extend {{{immutable.SortedMap[Foo, Any]}}}
	 *
	 * @tparam K  the type of the map keys.
	 * @tparam V  the type of the map values.
	 * @tparam MC the map's type constructor. */
	trait MapFormatDecider[K, -V, -MC[_, _]] {
		/** Advice: When possible, implement this method with a `val` to improve speed efficiency.
		 *
		 * @return the result determines the JSON format of the map: true -> object, false -> array. */
		def useObject: Boolean;
	}

	implicit def defaultMapFormatDecider[K](implicit ktt: ru.TypeTag[K]): MapFormatDecider[K, Any, MapUpperBound] =
		new MapFormatDecider[K, Any, MapUpperBound] {
			override val useObject: Boolean =
				ktt.tpe <:< ru.typeOf[CharSequence] || ktt.tpe =:= ru.definitions.IntTpe || ktt.tpe =:= ru.definitions.LongTpe || ktt.tpe =:= ru.definitions.CharTpe;
		}

	def apply[K, V, MC[k, v] <: MapUpperBound[k, v]](
		implicit
		ka: Appender[K],
		va: Appender[V],
		charSeqAppender: Appender[CharSequence],
		mfd: MapFormatDecider[K, V, MC]
	): Appender[MC[K, V]] = { (record, map) =>
		var isFirst = true;
		if (mfd.useObject) {
			record.append('{')

			if (ka == charSeqAppender) {
				map.foreach { e =>
					if (isFirst) {
						isFirst = false;
					} else {
						record.append(",");
					}
					record.appendSummoned[K](e._1)(ka)
						.append(':')
						.appendSummoned[V](e._2)(va)
				}
			} else {
				val keyRecord = new RecordStr(new lang.StringBuilder())
				map.foreach { e =>
					if (isFirst) {
						isFirst = false;
					} else {
						record.append(",");
					}
					keyRecord.sb.setLength(0);
					keyRecord.appendSummoned[K](e._1)(ka);

					charSeqAppender.append(record, keyRecord.sb);
					record.append(':')
						.appendSummoned[V](e._2)(va)
				}
			}
			record.append('}')

		} else {
			record.append('[')
			map.foreach { e =>
				if (isFirst) {
					isFirst = false;
					record.append('[');
				} else {
					record.append(",[")
				}
				record.appendSummoned[K](e._1)(ka)
					.append(',')
					.appendSummoned[V](e._2)(va)
					.append(']')
			}
			record.append(']')
		}
	}
}
