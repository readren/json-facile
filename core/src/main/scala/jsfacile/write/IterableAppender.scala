package jsfacile.write

import jsfacile.api.IterableUpperBound

object IterableAppender {

	def apply[E, IC[e] <: IterableUpperBound[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] = { (record, iterable) =>
		var isTail = false;
		record.append('[')
		iterable.foreach { e =>
			if (isTail) {
				record.append(',');
			}
			elemAppender.append(record, e);
			isTail = true;
		}
		record.append(']')
	}
}
