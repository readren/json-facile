package jsfacile.write

import java.lang
import scala.reflect.runtime.{universe => ru}

object MapAppender {
	trait MapFormatDecider[+K, +V, +MC] {
		def useObject: Boolean;
	}

	def defaultMapFormatDecider[K, V, M](implicit ktt: ru.TypeTag[K]): MapFormatDecider[K, V, M] = new MapFormatDecider[K, V, M] {
		override val useObject: Boolean =
			ktt.tpe <:< ru.typeOf[CharSequence] || ktt.tpe <:< ru.typeOf[Int] || ktt.tpe <:< ru.typeOf[Long] || ktt.tpe <:< ru.typeOf[Char];
	}

	def apply[K, V, MC[k, v] <: Map[k, v]](
		implicit
		ka: Appender[K],
		va: Appender[V],
		mfd: MapFormatDecider[K, V, MC[K, V]] = defaultMapFormatDecider
	): Appender[MC[K, V]] = { (record, map) =>
		var isFirst = true;
		if (mfd.useObject) {
			record.append('{')

			if (ka == jsfacile.write.jaCharSequence) {
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

					jsfacile.write.jaCharSequence.append(record, keyRecord.sb);
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
