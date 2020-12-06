package jsfacile.write


object ArrayAppender {

	@inline def apply[E](implicit elemAppender: Appender[E]): Appender[Array[E]] = { (record, array) =>
		record.append('[')

		if (array.length > 0) {
			elemAppender.append(record, array(0))
		}
		var i = 1;
		while (i < array.length) {
			record.append(',');
			elemAppender.append(record, array(i));
			i += 1;
		}

		record.append(']')
	}
}
