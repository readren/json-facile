package jsfacile.write

import jsfacile.joint.AnyAdt

object PrefixInserter {

	/** Summons an instance of [[PrefixInserter]] for the specified type `A`. */
	def apply[A, F <: AnyAdt](implicit cfd: PrefixInserter[A, F]): PrefixInserter[A, F] = cfd
}

/** When an instance of this class is in the implicit scope when an [[Appender]] is automatically derived, the [[Appender.append]] method of said [[Appender]] will insert after the discriminator field the JSON fragment returned by the [[fragment]] method.
 * Useful to make the automatically derived [[Appender]]s include fields that are common to a set of classes.
 *
 * @tparam A only the appenders for `A` and subtypes of `A` will be affected by this [[PrefixInserter]].
 * @tparam F determines which kind of [[Appender]]s are affected: appenders for products ([[jsfacile.joint.ProductsOnly]], for coproducts [[jsfacile.write.PrefixInserter.CoproductsOnly]], or both [[jsfacile.write.PrefixInserter.AnyAdt]]. */
trait PrefixInserter[-A, -F <: AnyAdt] {

	/** The fragment of JSON to insert between the type-discriminator field (if it is included) and the other fields.
	 *  @param value      the instance being appended
	 * @param isCoproduct `true` if the calling appender is for a coproduct. `false` if calling appender is for a product. Useful to know the kind of the calling appender when the type parameter `F` is [[AnyAdt]].
	 * @param symbol      the symbol name of the actual type of the received value.
	 * @return the fragment of JSON to insert after the discriminator field. The commas that separate the returned segment from the surrounding fields are automatically added.*/
	def fragment(value: A, isCoproduct: Boolean, symbol: String): String

}

