package write


object IterableAppender {

	def apply[E, IC[e] <: Iterable[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] = { (record, iterable) =>
		var isTail = false;
		record.append('[')
		iterable.foreach { e =>
			if(isTail) {
				record.append(',');
			}
			elemAppender.append(record, e);
			isTail = true;
		}
		record.append(']')
	}
}
