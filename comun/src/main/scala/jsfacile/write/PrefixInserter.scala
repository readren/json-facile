package jsfacile.write

import jsfacile.joint.AnyAdt

object PrefixInserter {

	/** Summons an instance of [[PrefixInserter]] for the specified type `A`. */
	def apply[A, F <: AnyAdt](implicit cfd: PrefixInserter[A, F]): PrefixInserter[A, F] = cfd
}

/** When an instance of this class is within the implicit scope of an automatic [[Appender]] deriver, the [[jsfacile.write.PrefixInserter.insert]] method is called by it after the discriminator field was appended (if applicable), and before any field is appended.
 * Useful to make the automatic [[Appender]] for concrete ADTs (products) include a discriminator, or fields that are common to a set of classes.
 * Note that this is not a low level replacement of [[jsfacile.joint.DiscriminatorDecider]] for abstract ADTs (coproducts).
 * @tparam A type for which the automatic [[Appender]] would be affected by this [[PrefixInserter]] instance. Subtypes are affected also.
 * @tparam F determines which kind of [[Appender]]s are affected: appenders for products ([[jsfacile.joint.ProductsOnly]], for coproducts [[jsfacile.write.PrefixInserter.CoproductsOnly]], or both [[jsfacile.write.PrefixInserter.AnyAdt]]. */
trait PrefixInserter[-A, -F <: AnyAdt] {

	/** @param record     the record where the calling [[jsfacile.write.Appender]] is appending the value
	 * @param value       the instance being appended
	 * @param isCoproduct `true` if the calling appender is for a coproduct, which happens when `A` is abstract. `false` if corresponds to a product, which happens when `A` is concrete.
	 * @param symbol      the symbol name of the actual type of the received value. This name is the same used by automatically derived [[Appender]]s for the value of the discriminator field. */
	def insert(record: Record, value: A, isCoproduct: Boolean, symbol: String): Boolean

}

