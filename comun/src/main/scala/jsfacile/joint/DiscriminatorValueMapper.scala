package jsfacile.joint

trait DiscriminatorValueMapper[C, F <: AnyAdt] {
	def apply(symbolName: String): String;
}
