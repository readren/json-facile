package jsfacile.api

package object builder {

	/** Instances of this trait are created by the [[jsfacile.api.builder.CoproductTranslatorsBuilder.ProductParsingInfoBuilder.complete]] method.
	 * The information produced by said method is not contained in the returned [[ProductParsingInfo]] instance itself but in a mirror container that exists during compilation time only. Instances of this class are a kind of runt-time manifestation of a compile-time information container.
	 * Said container contains the information supplied to said builder instance, which is what a [[jsfacile.builder.CoproductTranslatorsBuilder]][C] needs about a direct subtype `P` of `C` to build a [[Parser]][C]. `P` stands for "product" and `C` stands for "coproduct".
	 *  */
	trait ProductParsingInfo[P]
}
