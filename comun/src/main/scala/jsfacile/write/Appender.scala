package jsfacile.write

object Appender {
	/** Appender summoner */
	def apply[A](implicit appender: Appender[A]): Appender[A] = appender;

	/** Creates an [[Appender]][B] from an [[Appender]][A] and a function from `B` to `A`. */
	def convert[A, B](f: B => A)(implicit appenderA: Appender[A]): Appender[B] = (r, b) => {
		r.appendSummoned(f(b))(appenderA)
	}
}

/** Models a representation translator from scala object instance to JSON document.
 *
 * Note that the type parameter is non variant because, if it were, the covariance would cause trouble when using this trait as a type class contract, because Appender[B] is not necessarily a replacement of Appender[A] even if `B <: A`. For example, if A where `Iterable[(String, Int)]` and B where `Map[String, Int]`, the json appender for maps `Appender[Map[String, Int]]` won't be a good replacement of the json appender for iterables `Parser[Iterable[(String, Int)]]` because the map dependes on the [[MapFormatDecider]][String, Int, Map] found in the implicit scope, and the iterable not.
 *
 * A better solution would be to make this trait covariant and wrap it with a non variant wrapper when used as type class contract, but that would be more boilerplate. And the cost of making this trait non variant is low (I believe).
 * */
trait Appender[A] {
	/** Transforms an instance from scala to JSON and appends it to the received [[Record]].
	 *
	 * The implementation should always terminate normally (never throw any exception). */
	def append(record: Record, a: A): Record;

	/** A version of [[append]] that keeps the record's type. "Krt" stands for Keeping Record Type.
	 *
	 * This combinator should be the primitive one, but that would prevent to implement extensions with single abstract methods, because they aren't supported when the method is polymorphic. */
	def appendKrt[R <: Record](r: R, a: A): R = append(r, a).asInstanceOf[R];

	/** Creates an [[jsfacile.write.Appender]][B] from this [[jsfacile.write.Appender]] instance and a function from `B` to `A`. */
	def to[B](f: B => A): Appender[B] = Appender.convert[A, B](f)(this)
}
