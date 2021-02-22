package jsfacile.write

object Appender {
	/** Appender summoner */
	def apply[A](implicit appender: Appender[A]): Appender[A] = appender;

	/** Creates an [[Appender]][B] from and [[Appender]][A] and a function from `B` to `A`. */
	def convert[A, B](f: B => A)(implicit appenderA: Appender[A]): Appender[B] = (r, b) => {
		r.appendSummoned(f(b))(appenderA)
	}
}

trait Appender[A] {
	def append(record: Record, a: A): Record;

	/** A version of [[append]] that keeps the record's type. "Krt" stands for Keeping Record Type.
	 * This combinator should be the primitive one, but that would prevent to implement extensions with single abstract methods, because they aren't supported when the method is polymorphic. */
	def appendKrt[R <: Record](r: R, a: A): R = append(r, a).asInstanceOf[R];

	/** Creates an [[jsfacile.write.Appender]][B] from and this [[jsfacile.write.Appender]] instance and a function from `B` to `A`. */
	def to[B](f: B => A): Appender[B] = Appender.convert[A, B](f)(this)
}
