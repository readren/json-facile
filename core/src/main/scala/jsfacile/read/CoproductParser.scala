package jsfacile.read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import jsfacile.macros.{CoproductParserHelper, CoproductUpperBound, Named}
import jsfacile.macros.CoproductParserHelper.CphProductInfo
import jsfacile.util.BinarySearch

object CoproductParser {
	private case class Field[+V](name: String, value: V) extends Named;

	private def definedFieldsNamesIn(fields: Iterable[Field[Any]]): String = fields.map {_.name} mkString ", ";
}


class CoproductParser[C <: CoproductUpperBound](helper: CoproductParserHelper[C]) extends Parser[C] {
	import CoproductParser._;
	import Parser._;

	assert(helper != null); // Fails here when the macro expansion of CoproductParserHelper fails for some reason. Usually because a compilation error of the expanded code. To find the place in the log search the string "<empty>"

	/** Used by this parser to maintains the state of a product and known it's [[CphProductInfo]]. */
	private class Manager(val productInfo: CphProductInfo[C]) {
		var missingRequiredFieldsCounter: Int = productInfo.numberOfRequiredFields;
		var isViable: Boolean = true;
	}

	override def parse(cursor: Cursor): C = {


		if (cursor.have) {
			if (cursor.pointedElem == '{') {
				cursor.advance();

				val managers = helper.productsInfo.map(new Manager(_));
				val parsedFields = new ArrayBuffer[Field[Any]](math.min(ArrayBuffer.DefaultInitialSize, helper.fieldsParsers.length));

				var have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != '}') {
					have = false;
					val fieldName = jpString.parse(cursor);
					if (cursor.consumeWhitespaces() &&
						cursor.consumeChar(':') &&
						cursor.consumeWhitespaces()
					) {
						if (fieldName == helper.discriminator) {
							val productName = BasicParsers.jpString.parse(cursor);
							if (cursor.consumeWhitespaces()) {
								managers.foreach { m =>
									if (m.productInfo.name != productName)
										m.isViable = false;
								}
								have = true
							}

						} else {
							val fieldParser = BinarySearch.find(helper.fieldsParsers)(_.name.compareTo(fieldName));
							if (fieldParser != null) {
								// as side effect, actualize the product's managers
								var managerIndex = managers.size - 1;
								while (managerIndex >= 0) {
									val manager = managers(managerIndex);
									if (manager.isViable) {
										val fieldInfo = BinarySearch.find(manager.productInfo.fields)(_.name.compareTo(fieldName));
										if (fieldInfo == null) {
											manager.isViable = false;
										} else if (fieldInfo.oDefaultValue.isEmpty) {
											manager.missingRequiredFieldsCounter -= 1
										}
									}
									managerIndex -= 1;
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
					managers.foreach { manager =>
						if (manager.isViable && manager.missingRequiredFieldsCounter == 0) {
							if (chosenManager != null) {
								isAmbiguous = true;
							} else {
								chosenManager = manager;
							}
						}
					}
					if (chosenManager == null) {
						cursor.miss(s"There is no product extending ${helper.fullName} with all the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(parsedFields)}. Note that only the fields that are defined in at least one of said products are considered.")
					} else if (isAmbiguous) {
						cursor.fail(s"""Ambiguous products: more than one product of the coproduct "${helper.fullName}" has the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(parsedFields)}; and the viable products are: ${managers.iterator.filter(m => m.isViable && m.missingRequiredFieldsCounter == 0).map(_.productInfo.name).mkString(", ")}.""");
					} else {
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
					cursor.fail(s"Invalid syntax for an object while parsing a ${helper.fullName}");
				}
			} else {
				cursor.miss(s"A '{' was expected but '${cursor.pointedElem}' was found when trying to parse a ${helper.fullName}")
			}
		} else {
			cursor.miss(s"A '{' expected but the end of the content was reached when trying to parse a ${helper.fullName}")
		}
		ignored[C]
	}

}
