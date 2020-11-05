package jsfacile.read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import jsfacile.joint.CoproductUpperBound
import jsfacile.macros.CoproductParserHelper
import jsfacile.macros.CoproductParserHelper.CphProductInfo
import jsfacile.read.SyntaxParsers._

object CoproductParser {
	private case class Field[+V](name: String, value: V);

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
				val foundFields = new ArrayBuffer[Field[Any]](math.min(ArrayBuffer.DefaultInitialSize, helper.fieldsInfo.size));

				var have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != '}') {
					have = false;
					val fieldName = string.parse(cursor);
					if (cursor.consumeWhitespaces() &&
						cursor.consumeChar(':') &&
						cursor.consumeWhitespaces()
					) {
						if (fieldName == helper.discriminator) {
							val productName = PrimitiveParsers.jpString.parse(cursor);
							if (cursor.consumeWhitespaces()) {
								managers.foreach { m =>
									if (m.productInfo.name != productName)
										m.isViable = false;
								}
								have = true
							}

						} else {
							helper.fieldsInfo.get(fieldName) match { // TODO use binary search instead, to avoid the Option instance creation
								case Some(fieldValueParser) =>
									// as side effect, actualize the product's managers
									managers.foreach { manager =>
										val index = manager.productInfo.fields.indexWhere(_.name == fieldName);
										if (index < 0) {
											manager.isViable = false;
										} else if (manager.productInfo.fields(index).oDefaultValue.isEmpty) {
											manager.missingRequiredFieldsCounter -= 1
										}
									}
									// parse the field value
									val fieldValue = fieldValueParser.parse(cursor);
									if (cursor.consumeWhitespaces()) {
										foundFields.addOne(Field(fieldName, fieldValue))
										have = true
									}

								case None =>
									skipJsValue(cursor)
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
						cursor.fail(s"There is no product extending ${helper.fullName} with all the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(foundFields)}. Note that only the fields that are defined in at least one of said products are considered.")
					} else if (isAmbiguous) {
						cursor.fail(s"""Ambiguous products: more than one product of the coproduct "${helper.fullName}" has the fields contained in the json object being parsed. The contained fields are: ${definedFieldsNamesIn(foundFields)}; and the viable products are: ${managers.iterator.filter(m => m.isViable && m.missingRequiredFieldsCounter == 0).map(_.productInfo.name).mkString(", ")}.""");
					} else {
						// build the arguments lists of the product's primary constructor
						val chosenProductFields = chosenManager.productInfo.fields;
						val ctorArgs: Array[Any] = new Array(chosenProductFields.size);
						var argIndex: Int = 0;
						while (argIndex < chosenProductFields.size) {
							val fieldInfo = chosenProductFields(argIndex);
							val matchingParsedFieldIndex = foundFields.indexWhere(_.name == fieldInfo.name);
							ctorArgs(argIndex) = if (matchingParsedFieldIndex >= 0) {
								foundFields(matchingParsedFieldIndex).value;
							} else {
								fieldInfo.oDefaultValue.get;
							}
							argIndex += 1;
						}
						return chosenManager.productInfo.constructor(ArraySeq.unsafeWrapArray(ctorArgs));
					}

				} else {
					cursor.fail(s"Invalid syntax for an object while parsing a ${helper.fullName}");
				}
			} else {
				cursor.fail(s"A '{' was expected but '${cursor.pointedElem}' was found when trying to parse a ${helper.fullName}")
			}
		} else {
			cursor.fail(s"A '{' expected but the end of the content was reached when trying to parse a ${helper.fullName}")
		}
		ignored[C]
	}

}
