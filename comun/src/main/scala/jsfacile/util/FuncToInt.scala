package jsfacile.util

/** A specialization of `Function1` for results of typ `Int` to avoid the boxing and unboxing of the result. */
trait FuncToInt[@specialized(Int) A] {
	def apply(a: A): Int
}
