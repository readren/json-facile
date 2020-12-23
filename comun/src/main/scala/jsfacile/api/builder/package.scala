package jsfacile.api

import jsfacile.write.Appender

package object builder {

	/** Type of the result of the [[jsfacile.api.builder.CoproductBuilder.ProductParsingInfoBuilder.complete]] method.
	 * The information returned by said method is not kept in this instance itself but in a mirror container that exists during compilation time only. Instances of this class are a kind of runt-time manifestation of a compile-time information container.
	 * Said container contains the information that a [[jsfacile.builder.CoproductBuilder]][C] needs about a direct subtype `P` of `C` to build a [[Parser]][C]. `P` stands for "product" and `C` stands for "coproduct".
	 *  */
	class ProductParsingInfo[P]()

	/**Knows the information that a [[jsfacile.builder.CoproductBuilder]][C] needs about the product `P`, which must be a direct subtype of `C`, to build an [[Appender]][C]. `P` stands for "product" and `C` stands for "coproduct"
	 * @tparam P the type of the product
	 * @param appender the appender that will be used to append instances of `P`. This [[Appender]][P] must meet these requirements: It must generate single JSON object; that object must contain all the fields specified in the `requiredFieldNames` parameter; and it must include a discriminator field if the set of required fields names is equal to a sibling product (another subtype of `C`).
	 * @param requiredFieldNames the names of the fields that will ever be present in the JSON resulting from applying the `appender`. This information is needed by the derived appenders generator ([[CoproductAppenderMacro]] to know if it has to include a discriminator field to the derived appenders of the siblings of `P` (other subtypes of `C`). */
	case class ProductAppendingInfo[P](appender: Appender[P])(requiredFieldNames: String*)

}
