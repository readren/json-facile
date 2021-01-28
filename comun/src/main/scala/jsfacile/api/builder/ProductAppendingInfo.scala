package jsfacile.api.builder

import jsfacile.write.Appender


object ProductAppendingInfo {
	/** Template that specifies the form of the expression that must be supplied as argument to the `appendingInfo` parameter of the [[jsfacile.api.builder.CoproductTranslatorsBuilder.add*]] methods.
	 *
	 * @tparam P the type of the product
	 * @param appender the appender that will be used to append instances of `P`. This [[Appender]][P] must meet the following requirements: It must generate single JSON object; that object must contain all the fields specified in the `requiredFieldNames` parameter; and it must include a discriminator field if the set of required fields names is equal to a sibling product (another subtype of `C`).
	 * @param requiredFieldNames the names of the fields that will ever be present in the JSON resulting from applying the `appender`. This information is needed by the derived appenders generator ([[CoproductAppenderMacro]] to know if it has to include a discriminator field to the derived appenders of the siblings of `P` (other subtypes of `C`). */
	def apply[P](appender: Appender[P])(requiredFieldNames: String*): ProductAppendingInfo[P] = throw new AssertionError("This method signature is an expression template. It is not intended to be called, but parsed by a macro.")

}



/** Knows the information that a [[jsfacile.builder.CoproductTranslatorsBuilder]][C] needs about the product `P`, which must be a direct subtype of `C`, to build an [[Appender]][C]. `P` stands for "product" and `C` stands for "coproduct" */
trait ProductAppendingInfo[P]
