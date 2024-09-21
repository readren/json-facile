package jsfacile.api.builder

import jsfacile.api.{Appender, Parser}
import jsfacile.macros.macrosEntrance

/** A builder of instances of [[Parser]][C] and/or [[Appender]][C] for an abstract data type `C`. The ADT `C` may be non sealed.
 *
 * The names of the type parameters `P` and `C` stand for "product" and "coproduct" respectively. A product is a concrete data type. A coproduct is an abstract data type.
 *
 * Caution: The state of instances of the same type is shared. Therefore, at most one instance of this class per type of the `C` parameter should be used simultaneously. Use the [[clear]] method if you need to build two different translators for the same type.
 * TODO Use vampire methods (https://meta.plasm.us/posts/2013/07/12/vampire-methods-for-structural-types/) to remove the weird behaviour and limitations of this class.
 *
 * @define addProduct Adds the concrete data type `P` to the set of considered subtypes of `C`
 * @define compileOrderLimitation Limitation: Only are considered the calls to [[add*]] that were compiled before this method call. Passing an instance of this builder to a function whose body calls some [[add*]] method has an unspecified behaviour.
 *
 * @tparam C the type of the coproduct. */
class CoproductTranslatorsBuilder[C] {

	/** A builder of instances of [[ProductParsingInfo]], which are required by the [[CoproductTranslatorsBuilder.add*]] methods that customize the parser.
	 *
	 * @tparam P the subtype of `C` this builder builds a [[ProductParsingInfo]] for. */
	class ProductParsingInfoBuilder[P] {

		/** Adds the information required by the [[ProductParsingInfoBuilder]][C] about a field of the subtype `P`.
		 *
		 * @tparam F the type of the field. This type parameter is mandatory.
		 * @param name the name of the field in the JSON representation
		 * */
		def add[F](name: String): Unit = macro macrosEntrance.addFieldToParsingInfo[C, P, F]

		/** Adds the information required by the [[ProductParsingInfoBuilder]][C] about a field of the subtype `P`.
		 *
		 * @tparam F the type of the field. This type parameter is mandatory.
		 * @param name the name of the field in the JSON representation
		 * @param defaultValue the value assigned to this field when it is absent in the JSON representation.
		 * */
		def add[F](name: String, defaultValue: Option[F] = None): Unit = macro macrosEntrance.addFieldToParsingInfoWithDefaultValue[C, P, F]

		/**Creates a [[ProductParsingInfo]][P] which is a container of the information collected by this builder. Said container is what the [[CoproductTranslatorsBuilder.add*]] methods take to customize the parsing.
		 * The value of the discriminator field associated to the subtype `P` would be the name of the class that constructs it. Note that the name of the discriminator field is governed by the [[jsfacile.api.DiscriminatorDecider]][P] in the implicit scope.
		 *
		 * $compileOrderLimitation
		 * @param ctor a function that takes the values parsed from the fields specified by the previous calls to the [[add*]] methods; and returns an instance of `P`. The order of the parameters is determined by the order of the calls to the [[add*]] methods.
		 * */
		def complete(ctor: Seq[Any] => P): ProductParsingInfo[P] = macro macrosEntrance.completeProductParsingInfo[C, P]

		/**Creates a [[ProductParsingInfo]][P] which is a container of the information collected by this builder. Said container is what the [[CoproductTranslatorsBuilder.add*]] methods take to customize the parsing.
		 *
		 * $compileOrderLimitation
		 * @param ctor a function that takes the values parsed from the fields specified by the previous calls the [[add*]] methods; and returns an instance of `P`. The order of the parameters is determined by the order of the calls to the [[add*]] methods.
		 * @param discriminatorValue the discriminator field value associated to the product `P`. When the JSON object being parsed contains a discriminator field and its value equals the value of this parameter, the [[CoproductParser]][C] will use the [[ProductParsingInfo]] returned by this method to create the instance of `P`. This name must be unique between all the considered subtypes of the coproduct. Note that the name of the discriminator field is governed by the [[jsfacile.api.DiscriminatorDecider]][P] in the implicit scope.
		 * */
		def complete(discriminatorValue: String)(ctor: Seq[Any] => P): ProductParsingInfo[P] = macro macrosEntrance.completeProductParsingInfoSpecifyingDiscriminatorValue[C, P]
	}

	/** Creates a new [[ProductParsingInfoBuilder]][P] which is needed when the parser of `P` is customized. This builder is required to build the instance of [[ProductParsingInfo]][P] required by the [[CoproductTranslatorsBuilder.add*]] methods that customize the parsing. */
	def productParsingInfoBuilder[P]: ProductParsingInfoBuilder[P] = new ProductParsingInfoBuilder[P];


	class ProductAppendingInfoBuilder[P] {
		/** Adds the information required by the [[ProductAppendingInfoBuilder]][C] about a field of the product `P`.
		 * @tparam F the type of the field. This type parameter is mandatory.
		 * @param name the name of the field in the JSON representation
		 * @param accessor a function that obtains the value of the field from an instance of the product `P`. */
		def add[F](name: String, accessor: P => F): Unit = macro macrosEntrance.addFieldToAppendingInfo[C, P, F]

		/**Creates a [[ProductAppendingInfo]][P] which is a container of the information collected by this builder. Said container is what the [[CoproductTranslatorsBuilder.add*]] methods take to customize the appending.
		 * The value of the discriminator field associated to the subtype `P` would be the name of the class that constructs it. Note that the inclusion of the discriminator field in the JSON document is governed by the [[jsfacile.api.DiscriminatorDecider]][P] in the implicit scope.
		 * 
		 * $compileOrderLimitation
		 * */
		def complete: ProductAppendingInfo[P] = macro macrosEntrance.completeProductAppendingInfo[C, P]

		/**Creates a [[ProductAppendingInfo]][P] which is a container of the information collected by this builder. Said container is what the [[CoproductTranslatorsBuilder.add*]] methods take to customize the appending.
		 *
		 * $compileOrderLimitation
		 * @param discriminatorValue the discriminator field value associated to the subtype `P`. This value must be unique between all the considered subtypes of the coproduct `C`. Note that the inclusion of the discriminator in the JSON document is governed by the [[jsfacile.api.DiscriminatorDecider]][P] in the implicit scope.
		 * */
		def complete(discriminatorValue: String): ProductAppendingInfo[P] = macro macrosEntrance.completeProductAppendingInfoSpecifyingDiscriminatorValue[C, P]
	}

	/** Creates a new [[ProductAppendingInfoBuilder]][P] which is an alternative for the creation of the [[ProductAppendingInfo]][P] instance required by the [[CoproductTranslatorsBuilder.add*]] methods that customize the appending.
	 * The other alternative is to put a literal call to the [[jsfacile.api.builder.ProductAppendingInfo.apply]] method as argument of the `appendingInfo` parameter of the [[CoproductTranslatorsBuilder.add*]] method.
	*/
	def productAppendingInfoBuilder[P]: ProductAppendingInfoBuilder[P] = new ProductAppendingInfoBuilder[P]

	/** Adds the data type `T` to the set of considered subtypes of `C`.
	 * `T` can be a concrete or a sealed abstract data type.
	 * If concrete, its translators are derived automatically from its primary constructor.
	 * If abstract, the translators of all its concrete subtypes are derived automatically from their primary constructor.
	 *
	 * The type parameter is mandatory.
	 * @tparam T the subtype of C to add to the set of considered subtypes of `C`.
	 *  */
	def add[T <: C](): Unit = macro macrosEntrance.addCase[C, T];

	/** $addProduct customizing the [[Appender]][P] used by the [[Appender]][C] when the value to append is and instance of `P`.
	 * The [[Parser]][P] is derived automatically but the [[Appender]][P] is determined by the provided [[ProductAppendingInfo]].
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P <: C](appendingInfo: ProductAppendingInfo[P]): Unit = macro macrosEntrance.addCaseWithAppender[C, P];

	/** $addProduct customizing the discrimination and the [[Parser]][P] used by the [[Parser]][C] when the JSON fragment being parsed is a representation of `P`.
	 * The [[Appender]][P] is derived automatically but the [[Parser]][P] is determined by the provided [[ProductParsingInfo]], which must have been created by the [[ProductParsingInfoBuilder.complete*]] method.
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P <: C](parsingInfo: ProductParsingInfo[P]): Unit = macro macrosEntrance.addCaseWithParser[C, P];

	/** $addProduct customizing both, the [[Appender]][P] used by the [[Appender]][C] when the value to append is and instance of `P`; and the [[Parser]][P] used by the [[Parser]][C] when the JSON fragment being parsed is a representation of `P`.
	 * The [[Appender]][P] is determined by the provided [[ProductAppendingInfo]].
	 * The [[Parser]][P] is determined by the provided [[ProductParsingInfo]], which must have been created by one of the [[ProductParsingInfoBuilder.complete*]] methods.
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P <: C](appendingInfo: ProductAppendingInfo[P], parsingInfo: ProductParsingInfo[P]): Unit = macro macrosEntrance.addCaseWithBoth[C, P];

	/** Clears all the information supplied to this builder, leaving it as if it were just created.
	 *
	 * Useful when you want to build two or more different translators for the same type. Note that using another instance would not work because the state of all instances of [[CoproductTranslatorsBuilder]][X] is shared. This is an extraordinary behaviour.
	 * */
	def clear(): Unit = macro macrosEntrance.clearCases[C];

	/** Creates a [[Parser]][C] considering the information previously supplied with the [[add*]] methods calls.
	 * 
	 * $compileOrderLimitation */
	def parser: Parser[C] = macro macrosEntrance.sealParser[C];

	/** Creates an [[Appender]][C] considering the information previously supplied with the [[add*]] methods calls.
	 * 
	 * $compileOrderLimitation */
	def appender: Appender[C] = macro macrosEntrance.sealAppender[C];

}
