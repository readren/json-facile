package jsfacile.read

import scala.collection.immutable.ArraySeq

import jsfacile.macros.{CoproductUpperBound, Named}
import jsfacile.read.CoproductParser.{CphFieldParser, CphProductInfo}
import jsfacile.util.{BinarySearch, Pool}

object CoproductParser {

	final case class CphFieldInfo(name: String, argIndex: Int, oDefaultValue: Option[Any]) extends Named;

	/** @param name                  the product name
	 * @param numberOfRequiredFields the number of required fields of the product this instance represents.
	 * @param fields                 the info of the fields of the product this instance represents, sorted by the [[CphFieldInfo.name]].
	 * @param constructor            a function that creates an instance of the product this instance represents calling the primary constructor of said product. */
	final case class CphProductInfo[+P](name: String, numberOfRequiredFields: Int, fields: Array[CphFieldInfo], constructor: Seq[Any] => P) extends Named;

	final case class CphFieldParser(name: String, parser: Parser[_]) extends Named;

	/** Maintains the parsing state of a considered field. Also knows the [[CphFieldParser]] of the associated field. */
	private class Field(val fieldParser: CphFieldParser) extends Named {
		override def name: String = fieldParser.name;
		var wasParsed: Boolean = _;
		var value: Any = _;

		@inline def init(): Unit = {
			wasParsed = false
			value = null;
		}
	}

	private def definedFieldsNamesIn(fields: Array[Field]): String = fields.collect { case f if f.wasParsed => f.name } mkString ", ";

}


class CoproductParser[C <: CoproductUpperBound](
	fullName: String,
	discriminator: String,
	productsInfo: ArraySeq[CphProductInfo[C]],
	fieldsParsers: Array[CphFieldParser]
) extends Parser[C] {
	import CoproductParser._
	import Parser._;

	/** Used by this parser to maintain the parsing state of a product. Also knows the [[CphProductInfo]] of the associated product. */
	private class Manager(val productInfo: CphProductInfo[C]) {
		var missingRequiredFieldsCounter: Int = _;
		var isViable: Boolean = _;
		val fieldsValues: Array[Any] = new Array(productInfo.fields.length);
		val foundFields: Array[Boolean] = new Array(fieldsValues.length);

		@inline def init(): Unit = {
			missingRequiredFieldsCounter = productInfo.numberOfRequiredFields;
			this.isViable = true;

			var i = fieldsValues.length;
			while (i > 0) {
				i -= 1;
				fieldsValues(i) = null;
				foundFields(i) = false;
			}
		}
	}

	/** Contains all the local variables of a [[parse]] method execution.
	 * The local variables are reused to minimize transient object creation and garbage generation */
	private class Workplace {
		// create a manager for each CphProductInfo contained in the productsInfo array.
		val managers: Array[Manager] = {
			var mi = productsInfo.length
			val managers = new Array[Manager](mi)
			while (mi > 0) {
				mi -= 1;
				managers(mi) = new Manager(productsInfo(mi));
			}
			managers;
		};
		// create a considered field for each CphFieldParser contained in the fieldParsers array.
		val consideredFields: Array[Field] = Array.tabulate[Field](fieldsParsers.length) { i => new Field(fieldsParsers(i)) };
	}

	private val workplacesPool: Pool[Workplace] = new Pool();

	private implicit val workplaceAllocator: workplacesPool.Allocator = new workplacesPool.Allocator {
		override def alloc: Workplace = new Workplace
	}


	override def parse(cursor: Cursor): C = {

		if (cursor.have) {
			if (cursor.pointedElem == '{') {
				cursor.advance();

				val userId = workplacesPool.registerUser();
				val wp = workplacesPool.borrow(userId)

				// initialize the manager instances
				var mi = wp.managers.length
				while (mi > 0) {
					mi -= 1;
					wp.managers(mi).init();
				}
				// initialize the considered field instances
				mi = wp.consideredFields.length;
				while (mi > 0) {
					mi -= 1;
					wp.consideredFields(mi).init();
				}

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
							if (cursor.consumeWhitespaces()) {
								// make all the managers not viable except the one whose name equals the discriminator value
								mi = wp.managers.length;
								while (mi > 0) {
									mi -= 1;
									val m = wp.managers(mi);
									if (m.productInfo.name != productName)
										m.isViable = false;
								}
								have = true
							}

						} else {
							val consideredField = BinarySearch.find(wp.consideredFields)(_.name.compareTo(fieldName));
							if (consideredField != null) {
								// as side effect, actualize all the viable product's managers: mark as not vialbe those that don't have a field named `fieldName`; and actualize the `missingRequiredFields` counter.
								mi = wp.managers.length;
								while (mi > 0) {
									mi -= 1;
									val manager = wp.managers(mi);
									if (manager.isViable) {
										val fieldInfo = BinarySearch.find(manager.productInfo.fields)(_.name.compareTo(fieldName));
										if (fieldInfo == null) {
											manager.isViable = false;
										} else if (fieldInfo.oDefaultValue.isEmpty) {
											manager.missingRequiredFieldsCounter -= 1
										}
									}
								}
								// parse the field value
								val fieldValue = consideredField.fieldParser.parser.parse(cursor);
								if (cursor.consumeWhitespaces()) {
									consideredField.value = fieldValue;
									consideredField.wasParsed = true;
									have = true
								}

							} else {
								Skip.jsValue(cursor)
								have = cursor.consumeWhitespaces()

							}
						}
						have &&= cursor.pointedElem == '}' || (cursor.consumeChar(',') && cursor.consumeWhitespaces())
					}
				}
				// At this point all fields had been parsed. What continues determines to which product them belong, then uses the field values to build the arguments lists of the product's primary constructor, and finally calls said constructor to create the product instance.
				if (have) {
					cursor.advance();

					var chosenManager: Manager = null;
					var isAmbiguous: Boolean = false;

					// search the managers that are viable
					mi = wp.managers.length;
					while (mi > 0) {
						mi -= 1;
						val manager = wp.managers(mi);
						if (manager.isViable && manager.missingRequiredFieldsCounter == 0) {
							if (chosenManager != null) {
								isAmbiguous = true;
							} else {
								chosenManager = manager;
							}
						}
					}
					if (chosenManager == null) { // if none is viable
						cursor.miss(s"There is no product extending $fullName with all the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(wp.consideredFields)}. Note that only the fields that are defined in at least one of said products are considered.")
					} else if (isAmbiguous) { // if more than one is viable
						cursor.fail(s"""Ambiguous products: more than one product of the coproduct "$fullName" has the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(wp.consideredFields)}; and the viable products are: ${wp.managers.iterator.filter(m => m.isViable && m.missingRequiredFieldsCounter == 0).map(_.productInfo.name).mkString(", ")}.""");
					} else { // if only one is viable.
						// build the arguments lists of the product's primary constructor
						val chosenProductFields = chosenManager.productInfo.fields;


						var fieldInfoIndex = chosenProductFields.length;
						while (fieldInfoIndex > 0) {
							fieldInfoIndex -= 1;
							val fieldInfo = chosenProductFields(fieldInfoIndex);
							val consideredField = BinarySearch.find(wp.consideredFields)(_.name.compareTo(fieldInfo.name))
							chosenManager.fieldsValues(fieldInfo.argIndex) =
								if (consideredField != null && consideredField.wasParsed) {
									consideredField.value;
								} else {
									fieldInfo.oDefaultValue.get
								}
						}

						this.workplacesPool.unregisterUser(userId);
						return chosenManager.productInfo.constructor(ArraySeq.unsafeWrapArray(chosenManager.fieldsValues));
					}

				} else {
					cursor.fail(s"Invalid syntax for an object while parsing a $fullName");
				}
				this.workplacesPool.unregisterUser(userId);

			} else {
				cursor.miss(s"A '{' was expected but '${cursor.pointedElem}' was found when trying to parse a $fullName")
			}
		} else {
			cursor.miss(s"A '{' expected but the end of the content was reached when trying to parse a $fullName")
		}
		ignored[C]
	}
}
