package jsfacile.write

import jsfacile.api.{IterableUpperBound, MapUpperBound}
import jsfacile.write.MapAppender.MapFormatDecider

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.write]] package object implements it; and said package is where the [[Appender]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Appender]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityMediumAppenders extends PriorityLowAppenders {

	///////////////////////////////////////////////////////////////
	//// JSON appenders for standard collections library types ////

	@inline implicit def jaIterable[E, IC[e] <: IterableUpperBound[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] =
		IterableAppender.apply[E, IC](elemAppender)

	@inline implicit def jaMap[K, V, MC[k, v] <: MapUpperBound[k, v]](
		implicit
		ka: Appender[K],
		va: Appender[V],
		mfd: MapFormatDecider[K, V, MC]
	): Appender[MC[K, V]] =
		MapAppender.apply[K, V, MC](ka, va, mfd)

}
