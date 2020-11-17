package jsfacile.write

trait Record {
	def appendCodePoint(codePoint: Int): this.type;

	def append(csq: CharSequence): this.type;

	def append(csq: CharSequence, start: Int, `end`: Int): this.type;

	def append(char: Char): this.type;

	def append(string: String): this.type;

	def append(int: Int): this.type;

	def append(long: Long): this.type;

	def append(float: Float): this.type;

	def append(double: Double): this.type;

	def appendSummoned[T](t: T)(implicit appender: Appender[T]): this.type = {
		appender.append(this, t); // Fails with null pointer exception here when the macro expansion of ProductAppender fails for some reason. Usually because a compilation error of the expanded code. To find the place in the log search the string "<empty>"
		this
	};
}