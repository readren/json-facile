package jsfacile.read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import jsfacile.macros.{CoproductUpperBound, Named}
import jsfacile.read.CoproductParser.{CphFieldParser, CphProductInfo}
import jsfacile.util.BinarySearch

object CoproductParser {

	final case class CphFieldInfo(name: String, argIndex: Int, oDefaultValue: Option[Any]) extends Named;

	/** @param name                  the product name
	 * @param numberOfRequiredFields the number of required fields of the product this instance represents.
	 * @param fields                 the info of the fields of the product this instance represents, sorted by the [[CphFieldInfo.name]].
	 * @param constructor            a function that creates an instance of the product this instance represents calling the primary constructor of said product. */
	final case class CphProductInfo[+P](name: String, numberOfRequiredFields: Int, fields: Array[CphFieldInfo], constructor: Seq[Any] => P) extends Named;

	final case class CphFieldParser(name: String, parser: Parser[_]) extends Named;

	private case class Field[+V](name: String, value: V) extends Named;

	private def definedFieldsNamesIn(fields: Iterable[Field[Any]]): String = fields.map {_.name} mkString ", ";
}


class CoproductParser[C <: CoproductUpperBound](
	fullName: String,
	discriminator: String,
	productsInfo: ArraySeq[CphProductInfo[C]],
	fieldsParsers: Array[CphFieldParser]
) extends Parser[C] {
	import CoproductParser._
	import Parser._;

	/** Used by this parser to maintains the state of a product and known it's [[CphProductInfo]]. */
	private class Manager(val productInfo: CphProductInfo[C]) {
		var missingRequiredFieldsCounter: Int = productInfo.numberOfRequiredFields;
		var isViable: Boolean = true;
	}

	override def parse(cursor: Cursor): C = {

		if (cursor.have) {
			if (cursor.pointedElem == '{') {
				cursor.advance();

				// create a manager for each product
				var mi = productsInfo.length
				val managers = new Array[Manager](mi);
				while (mi > 0) {
					mi -= 1;
					managers(mi) = new Manager(productsInfo(mi));
				}
				val parsedFields = new ArrayBuffer[Field[Any]](math.min(ArrayBuffer.DefaultInitialSize, fieldsParsers.length));

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
								mi = managers.length;
								while (mi > 0) {
									mi -= 1;
									val m = managers(mi);
									if (m.productInfo.name != productName)
										m.isViable = false;
								}
								have = true
							}

						} else {
							val fieldParser = BinarySearch.find(fieldsParsers)(_.name.compareTo(fieldName));
							if (fieldParser != null) {
								// as side effect, actualize all the viable product's managers: mark as not vialbe those that don't have a field named `fieldName`; and actualize the `missingRequiredFields` counter.
								mi = managers.length;
								while (mi > 0) {
									mi -= 1;
									val manager = managers(mi);
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
								val fieldValue = fieldParser.parser.parse(cursor);
								if (cursor.consumeWhitespaces()) {
									parsedFields.addOne(Field(fieldName, fieldValue))
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
					mi = managers.length;
					while (mi > 0) {
						mi -= 1;
						val manager = managers(mi);
						if (manager.isViable && manager.missingRequiredFieldsCounter == 0) {
							if (chosenManager != null) {
								isAmbiguous = true;
							} else {
								chosenManager = manager;
							}
						}
					}
					if (chosenManager == null) { // if none is viable
						cursor.miss(s"There is no product extending $fullName with all the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(parsedFields)}. Note that only the fields that are defined in at least one of said products are considered.")
					} else if (isAmbiguous) { // if more than one is viable
						cursor.fail(s"""Ambiguous products: more than one product of the coproduct "$fullName" has the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(parsedFields)}; and the viable products are: ${managers.iterator.filter(m => m.isViable && m.missingRequiredFieldsCounter == 0).map(_.productInfo.name).mkString(", ")}.""");
					} else { // if only one is viable.
						// build the arguments lists of the product's primary constructor
						val chosenProductFields = chosenManager.productInfo.fields;
						val ctorArgsValues: Array[Any] = new Array(chosenProductFields.length);
						val ctorArgsFound: Array[Boolean] = new Array(chosenProductFields.length);

						// fill the chosen product's constructor arguments with the values found in the JSON document
						var parsedFieldIndex = parsedFields.size - 1;
						while (parsedFieldIndex >= 0) {
							val parsedField = parsedFields(parsedFieldIndex);
							val fieldInfo = BinarySearch.find(chosenProductFields)(_.name.compareTo(parsedField.name));
							ctorArgsValues.update(fieldInfo.argIndex, parsedField.value);
							ctorArgsFound.update(fieldInfo.argIndex, true);
							parsedFieldIndex -= 1;
						}
						// use default value for fields that are missing in the JSON document.
						var fieldInfoIndex = chosenProductFields.length - 1;
						while (fieldInfoIndex >= 0) {
							if (!ctorArgsFound(fieldInfoIndex)) {
								val fieldInfo = chosenProductFields(fieldInfoIndex);
								ctorArgsValues.update(fieldInfoIndex, fieldInfo.oDefaultValue.get);
							}
							fieldInfoIndex -= 1;
						}

						return chosenManager.productInfo.constructor(ArraySeq.unsafeWrapArray(ctorArgsValues));
					}

				} else {
					cursor.fail(s"Invalid syntax for an object while parsing a $fullName");
				}
			} else {
				cursor.miss(s"A '{' was expected but '${cursor.pointedElem}' was found when trying to parse a $fullName")
			}
		} else {
			cursor.miss(s"A '{' expected but the end of the content was reached when trying to parse a $fullName")
		}
		ignored[C]
	}
}
