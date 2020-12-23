package jsfacile.api.builder

import jsfacile.api.{Appender, Parser}
import jsfacile.macros.macrosEntrance

/** A builder that constructs instances of [[Parser]][C] and/or [[Appender]][C], where `C` is a data type with at least one subtype.
 * `C` may be non sealed.
 *
 * The names of the type parameters `P` and `C` stand for "product" and "coproduct" respectively.
 *
 * Caution 1: This class is not thread safe. Don't share an instance between concurrent threads.
 * Caution 2: It is allowed to use at most one instance of this class per type of the `C` parameter simultaneously.
 *
 * @define addProduct Adds the type `P` to the set of considered direct subtypes of `C`
 *
 * @tparam C the type of the coproduct. */
class CoproductBuilder[C] {

	/** A builder of instances of [[ProductParsingInfo]], which is needed to customize the [[Parser]][C] returned by the [[CoproductBuilder.parser]] method.
	 *
	 * @tparam P the type of the product this builder builds a [[ProductParsingInfo]] for. */
	class ProductParsingInfoBuilder[P] {

		/** Adds a field to the considered set of fields of `P`.
		 *
		 * The type parameter is mandatory.
		 * @tparam F the type of the field.
		 * */
		def add[F](name: String, defaultValue: Option[F] = None): Unit = macro macrosEntrance.addFieldToParsingInfo[C, P, F]

		/**Creates a [[ProductParsingInfo]][P] which is an container of the data required by the [[CoproductBuilder.add*]] methods that customize the parsing.
		 *
		 * @name the name of the type `P`. It is used when a discriminator field is included, in which case it takes this name as its value. Therefore, this name must be unique between all the considered subtypes of the coproduct.
		 * @ctor a function that takes the values parsed from the fields specified by the previous calls the [[add*]] methods; and returns an instance of `P`.*/
		def complete(name: String, ctor: Seq[Any] => P): ProductParsingInfo[P] = macro macrosEntrance.completeProductParsingInfo[C, P]
	}

	/** Creates a new [[ProductParsingInfoBuilder]][P] which is needed to build a [[ProductParsingInfo]][P]*/
	def productParsingInfoBuilder[P]: ProductParsingInfoBuilder[P] = new ProductParsingInfoBuilder[P];

	/** $addProduct.
	 * Both, the [[Parser]][P] and the [[Appender]][P] are derived automatically from the primary constructor of `P`.
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 *  */
	def add[P]: Unit = macro macrosEntrance.addCase[C, P];

	/** $addProduct customizing the [[Appender]][P] used by the [[Appender]][C] when the value to append is and instance `P`.
	 * The [[Parser]][P] is derived automatically but the [[Appender]][P] is determined by the provided [[ProdutAppendingInfo]].
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P](appendingInfo: ProductAppendingInfo[P]): Unit = macro macrosEntrance.addCaseWithAppender[C, P];

	/** $addProduct customizing the discrimination and the [[Parser]][P] used by the [[Parser]][C] especially when the JSON fragment being parsed corresponds to `P`. .
	 * The [[Appender]][P] is derived automatically but the [[Parser]][P] is determined by the provided [[ProductParsingInfo]].
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P](parsingInfo: ProductParsingInfo[P]): Unit = macro macrosEntrance.addCaseWithParser[C, P];

	/** $addProduct customizing both, the parsing aspect of the [[Parser[C]] and the appending aspect of the [[Appender]][C]
	 *
	 * The type parameter is mandatory.
	 * @tparam P the direct subtype of C to add to the set of considered subtypes of the coproduct.
	 * */
	def add[P](appendingInfo: ProductAppendingInfo[P], parsingInfo: ProductParsingInfo[P]): Unit = macro macrosEntrance.addCaseWithBoth[C, P];

	/** Creates a [[Parser]][C] considering the information previously supplied with the [[add*]] methods calls. */
	def parser: Parser[C] = macro macrosEntrance.sealParser[C];

	/** Creates an [[Appender]][C] considering the information previously supplied with the [[add*]] methods calls. */
	def appender: Appender[C] = macro macrosEntrance.sealAppender[C];

}
