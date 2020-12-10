package jsfacile.write

import java.lang

import jsfacile.joint.MapUpperBound

class MapAppender[K, V, MC[k, v] <: MapUpperBound[k, v]](
	ka: Appender[K],
	va: Appender[V],
	charSeqAppender: Appender[CharSequence],
	mfd: MapFormatDecider[K, V, MC]
) extends Appender[MC[K, V]] {
	override def append(record: Record, map: MC[K, V]): Record = {
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
