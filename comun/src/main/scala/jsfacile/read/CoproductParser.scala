package jsfacile.read

import scala.collection.immutable.ArraySeq

import jsfacile.joint.Named
import jsfacile.read.CoproductParser.{CpConsideredField, CpProductInfo}
import jsfacile.util.BitSet.{BitSlot, Shard}
import jsfacile.util.Pool.Allocator
import jsfacile.util.{BinarySearch, BitSet, Pool}

object CoproductParser {

	/** Info about a field of a product that implements the treated coproduct.
	 *
	 * @param name                 the name of the field
	 * @param consideredFieldIndex the index of this field in the `consideredFields` array before it is sorted.
	 * @param argIndex             the index of this field in the product primary constructor.
	 * @param oDefaultValue        the value to be used when the field is not present in the JSON document. When it is not defined, the field is required. */
	final case class CpFieldInfo(name: String, consideredFieldIndex: Int, argIndex: Int, oDefaultValue: Option[Any]) extends Named;

	/** The information about a product that the [[CoproductParser]] needs.
	 *
	 * @param name                 the product name. Used for the value of the discriminator field.
	 * @param requiredFieldsSet    the set of required fields of the product.
	 * @param requiredFieldsNumber number of fields of the product that are required.
	 * @param fields               the info of each field of the product, sorted by the [[CpFieldInfo.name]].
	 * @param constructor          a function that creates an instance of the product, calling the primary constructor of said product. */
	final case class CpProductInfo[+P](name: String, requiredFieldsSet: BitSet, requiredFieldsNumber: Int, fields: Array[CpFieldInfo], constructor: Seq[Any] => P) extends Named;

	/** Info about a considered field. The considered fields of a coproduct is the set of all the fields of the products contained by said coproduct grouped by the field name. The case when two considered fields have the same name but different type is not supported.
	 *
	 * @param name    the field name
	 * @param index   the index of this instance in the [[CoproductParser.consideredFields]] array before it is sorted.
	 * @param bitSlot the bit slot associated to this instance. Each instance has a different bit slot.
	 * @param parser  a [[Parser]] that parses JSON to instances of the type of the field this instance treats. */
	final case class CpConsideredField(name: String, index: Int, bitSlot: BitSlot, parser: Parser[_]) extends Named;

	/** Maintains the parsing state of a considered field. Also knows said considered field. */
	private class FieldState(val consideredField: CpConsideredField) extends Named {
		override def name: String = consideredField.name;
		var wasParsed: Boolean = _;
		var value: Any = _;

		@inline def init(): Unit = {
			wasParsed = false
			value = null;
		}
	}

	private def definedFieldsNamesIn(fields: Array[FieldState]): String = fields.collect { case f if f.wasParsed => f.name } mkString("{", ", ", "}")

}

/** A [[Parser]] from JSON object to an instance of `C`, where `C` is an abstract type.
 * This class is used by the code generated by the [[jsfacile.macros.CoproductParserMacro]] and designed for that purpose. But it may be used manually.
 * Primary constructor note: Despite some parameters are mutable, none is mutated.
 *
 * @tparam C the declared abstract type of the instances created by this [[Parser]].
 * @param fullName         the full name of the actual abstract type represented by the type parameter `C`. Only used for error messages.
 * @param discriminator    the name of the discriminator field. Used when disambiguation is necessary.
 * @param productsInfo     the information this [[Parser]] needs about all known concrete subtypes of `C`. It should be sorted by [[CpProductInfo.name]].
 * @param consideredFields an array with the information this [[Parser]] needs about all the fields from the JSON object that will be considered. It should be sorted by [[CpConsideredField.name]]. Any JSON object field whose name is not contained in this array would be ignored. The set of considered fields should be the union of the fields of the concrete implementations specified by the `productsInfo` parameter.
 * @param numberOfShards   number of [[Shard]] required to hold all the [[BitSlot]]s mentioned in the [[CpConsideredField]] instances contained by the `consideredFields` array. The necessity of this parameter is a consequence of the inability of the [[BitSet]] class to grow its shards array (because it is implemented with a value class). */
class CoproductParser[C](
	fullName: String,
	discriminator: String,
	productsInfo: Array[CpProductInfo[C]], // should be sorted by name. Not mutated.
	consideredFields: Array[CpConsideredField], // should be sorted by name. Not mutated.
	numberOfShards: Int
) extends Parser[C] {
	import CoproductParser._
	import Parser._;

	// create an array that maps from unsorted index to sorted index of the `consideredFields` array.
	private val indexMap = {
		val indexMap = new Array[Int](consideredFields.length);
		var i = consideredFields.length;
		while (i > 0) {
			i -= 1;
			indexMap(consideredFields(i).index) = i;
		}
		indexMap
	};


	/** Used by this parser to maintain the parsing state of a product. Also knows the [[CpProductInfo]] of the associated product. */
	private final class Manager(val productInfo: CpProductInfo[C]) extends Named {
		val name: String = productInfo.name;
		val fieldsValues: Array[Any] = new Array(productInfo.fields.length);
		val fieldsFound: Array[Boolean] = new Array(fieldsValues.length);

		@inline def init(): Unit = {
			var i = fieldsValues.length;
			while (i > 0) {
				i -= 1;
				fieldsValues(i) = null;
				fieldsFound(i) = false;
			}
		}
	}

	/** Contains all the local variables, of a [[parse]] method execution, that reference objects whose life ends at the end of the execution and the compiler escape analysis algorithm is not intelligent enough to move to the stack.
	 * Keeping a workplace allows to reuse said transient objects, minimizing object creation and garbage generation.
	 * Note that the introduction of the [[Workplace]] would make the [[CoproductParser]] loose it thread safety. That is solved storing it in a [[ThreadLocal]]. */
	private final class Workplace {
		// create a manager for each CpProductInfo contained in the productsInfo array.
		val managers: Array[Manager] = {
			var mi = productsInfo.length
			val managers = new Array[Manager](mi)
			while (mi > 0) {
				mi -= 1;
				managers(mi) = new Manager(productsInfo(mi));
			}
			managers;
		};
		// create a considered field for each CpConsideredField contained in the fieldParsers array.
		val consideredFieldsState: Array[FieldState] = Array.tabulate[FieldState](consideredFields.length) { i => new FieldState(consideredFields(i)) };

		val foundFields: BitSet = new BitSet(new Array[Shard](numberOfShards));
	}

	/** The workplace alone would be enough when the involved data type hierarchy is not recursive. The pool exists to support recursion. */
	private val workplacesPool: ThreadLocal[Pool[Workplace]] = ThreadLocal.withInitial[Pool[Workplace]](() => new Pool());

	private implicit val workplaceAllocator: Allocator[Workplace] = new Allocator[Workplace] {
		override def alloc: Workplace = new Workplace
	}


	override def parse(cursor: Cursor): C = {

		if (cursor.have) {
			if (cursor.pointedElem == '{') {
				cursor.advance();

				workplacesPool.get.borrowInsideOf { wp =>

					// initialize the Manager instances
					var index = wp.managers.length
					while (index > 0) {
						index -= 1;
						wp.managers(index).init();
					}
					// initialize the considered field instances
					index = wp.consideredFieldsState.length;
					while (index > 0) {
						index -= 1;
						wp.consideredFieldsState(index).init();
					}
					// initialize the foundFields set
					wp.foundFields.clear();

					var chosenManager: Manager = null

					var have = cursor.consumeWhitespaces();
					while (have && cursor.pointedElem != '}') {
						have = false;
						val fieldName = BasicParsers.jpString.parse(cursor);
						if (cursor.consumeWhitespaces() &&
							cursor.consumeChar(':') &&
							cursor.consumeWhitespaces()
						) {
							if (fieldName == discriminator) {
								val productName = BasicParsers.jpString.parse(cursor);
								chosenManager = BinarySearch.find(wp.managers)(_.name.compareTo(productName));
								if (chosenManager == null) {
									cursor.miss(s"""The discriminator field value "$productName" does not match the name of any of the concrete subclasses of $fullName.""")
								} else {
									have = cursor.consumeWhitespaces()
								}

							} else {
								val foundField = BinarySearch.find(wp.consideredFieldsState)(_.name.compareTo(fieldName));
								if (foundField != null) {
									val fieldParser = foundField.consideredField;

									// actualize the found-fields set
									wp.foundFields |= fieldParser.bitSlot;

									// parse the field value
									val fieldValue = fieldParser.parser.parse(cursor);
									foundField.value = fieldValue;
									foundField.wasParsed = true;

								} else {
									Skip.jsValue(cursor)
								}
								have = cursor.consumeWhitespaces()
							}
							have &&= cursor.pointedElem == '}' || (cursor.consumeChar(',') && cursor.consumeWhitespaces())
						}
					}

					if (have) {
						// At this point all fields of the JSON object had been parsed. What continues determines to which product them belong, then uses the field values to build the arguments lists of the product's primary constructor, and finally calls said constructor to create the product instance.

						cursor.advance(); // consume the closing '}'

						var isAmbiguous: Boolean = false;
						if (chosenManager != null) { // if the discriminator field is present
							if (!chosenManager.productInfo.requiredFieldsSet.isSubsetOf(wp.foundFields)) { // check all the fields required by the product pointed at are present
								cursor.miss(s"The JSON object being parsed does not contain all the fields required by ${chosenManager.productInfo.name}, which is the concrete subclass of $fullName that the discriminator field pointed at.");
								have = false;
							}
						} else { // if the discriminator field is absent chose the products such that all its required fields are present.
							index = wp.managers.length;
							while (index > 0) {
								index -= 1;
								val manager = wp.managers(index);
								val managerInfo = manager.productInfo;
								if (managerInfo.requiredFieldsSet.isSubsetOf(wp.foundFields)) {
									if (chosenManager == null || managerInfo.requiredFieldsNumber > chosenManager.productInfo.requiredFieldsNumber) {
										chosenManager = manager;
										isAmbiguous = false;
									} else if (managerInfo.requiredFieldsNumber == chosenManager.productInfo.requiredFieldsNumber) {
										isAmbiguous = true;
									}
								}
							}
						}

						if (chosenManager == null) { // if the discriminator fields is absent and no product has all its required fields present here
							cursor.miss(s"There is no concrete subclass of $fullName such that all its required fields are present in the JSON object being parsed. The present fields are: ${definedFieldsNamesIn(wp.consideredFieldsState)}. Note that only the fields that are defined in at least one of said products are considered.")
							ignored[C]
						} else if (isAmbiguous) { // if the discriminator fields is absent and more than product has all its required fields present here
							cursor.fail(s"""Ambiguous products: more than one concrete subclass of "$fullName" has the fields present in the json object being parsed. The present fields are: ${definedFieldsNamesIn(wp.consideredFieldsState)}; and the viable subclasses are: ${wp.managers.collect { case m if m.productInfo.requiredFieldsSet.isSubsetOf(wp.foundFields) => m.name }.mkString(", ")}.""");
							ignored[C]
						} else if (have) { // if the discriminator field is present or only one product has all its required fields present here
							val chosenProductFields = chosenManager.productInfo.fields;

							// build the arguments lists of the product's primary constructor
							index = chosenProductFields.length;
							while (index > 0) {
								index -= 1;
								val fieldInfo = chosenProductFields(index);
								val consideredField = wp.consideredFieldsState(indexMap(fieldInfo.consideredFieldIndex));
								chosenManager.fieldsValues(fieldInfo.argIndex) =
									if (consideredField.wasParsed) {
										consideredField.value;
									} else {
										fieldInfo.oDefaultValue.get
									}
							}

							chosenManager.productInfo.constructor(ArraySeq.unsafeWrapArray(chosenManager.fieldsValues));
						} else { // if the discriminator field is present but some required field is missing
							ignored[C]
						}

					} else {
						if (cursor.ok) cursor.fail(s"Invalid syntax for an object while parsing a $fullName");
						ignored[C]
					}
				}

			} else {
				cursor.miss(s"A '{' was expected but '${cursor.pointedElem}' was found when trying to parse a $fullName")
				ignored[C]
			}
		} else {
			cursor.miss(s"A '{' expected but the end of the content was reached when trying to parse a $fullName")
			ignored[C]
		}
	}
}
