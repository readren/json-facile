package jsfacile.write

import jsfacile.joint.IterableUpperBound

object IterableAppender {

	@inline def apply[E, IC[e] <: IterableUpperBound[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] = { (record, iterable) =>
		record.append('[');

		val i = iterable.iterator;
		if (i.hasNext) {
			elemAppender.append(record, i.next());
		}
		while (i.hasNext) {
			record.append(',');
			elemAppender.append(record, i.next());
		}

		record.append(']')
	}
}
