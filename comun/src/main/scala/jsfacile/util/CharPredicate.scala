package jsfacile.util

/** A specialization of [[scala.Function1]] to avoid the boxing and unboxing of both, the argument and the result. */
trait CharPredicate {
	def apply(c: Char): Boolean;
}
