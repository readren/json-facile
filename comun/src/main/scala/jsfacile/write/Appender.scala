package jsfacile.write

object Appender {
	/** Appender summoner */
	def apply[A](implicit appender: Appender[A]): Appender[A] = appender;
}


trait Appender[A] {
	def append(record: Record, a: A): Record;

	/** A version of [[append]] that keeps the record's type. "Krt" stands for Keeping Record Type.
	 * This combinator should be the primitive one, but that would prevent to implement extensions with single abstract methods, because they aren't supported when the method is polymorphic. */
	def appendKrt[R <: Record](r: R, a: A): R = append(r, a).asInstanceOf[R];
}
