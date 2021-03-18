package jsfacile.read

import scala.collection.immutable.ArraySeq

import jsfacile.read.Parser._
import jsfacile.read.ProductParser.{PpFieldInfo, PpHelper}
import jsfacile.util.BinarySearch

object ProductParser {

	final case class PpFieldInfo(name: String, valueParser: Parser[_], oDefaultValue: Option[Any], ctorArgIndex: Int)

	val fieldsOrdering: Ordering[PpFieldInfo] = Ordering.by(_.name);

	trait PpHelper[P] {
		def fullName: String;
		def fieldsInfo: Array[PpFieldInfo];
		def createProduct(args: Seq[Any]): P
	}

}

class ProductParser[P](helper: PpHelper[P]) extends Parser[P] {

	assert(helper != null); // Fails here when the macro expansion of CoproductParserMacro fails for some reason. Usually because a compilation error of the expanded code. To find the place in the log search the string "<empty>"

	override def parse(cursor: Cursor): P = {

		if (cursor.have) {
			if (cursor.pointedElem == '{') {
				cursor.advance();
				val fieldsCount = helper.fieldsInfo.length;
				// Efficiency comment: Apparently, this two arrays are stored in the stack thanks to the compiler escape analysis. Using the pool here has given no benefit.
				val ctorArgs = new Array[Any](fieldsCount);
				val fieldsFound = new Array[Boolean](fieldsCount);

				var have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != '}') {
					val fieldName = BasicParsers.jpString.parse(cursor);
					have = cursor.consumeWhitespaces() &&
							   cursor.consumeChar(':') &&
							   cursor.consumeWhitespaces();
					if (have) {
						val fieldInfo = BinarySearch.find[PpFieldInfo](helper.fieldsInfo) {_.name.compare(fieldName)}
						if (fieldInfo == null) {
							Skip.jsValue(cursor);
						} else {
							val fieldValue = fieldInfo.valueParser.parse(cursor);
							ctorArgs(fieldInfo.ctorArgIndex) = fieldValue;
							fieldsFound(fieldInfo.ctorArgIndex) = true;
						}
						have = cursor.consumeWhitespaces() && (cursor.pointedElem == '}' || (cursor.consumeChar(',') && cursor.consumeWhitespaces()))
					}
				}
				if (have) {
					cursor.advance();

					var fieldIndex = 0;
					while (fieldIndex < fieldsCount && cursor.ok) {
						if (!fieldsFound(fieldIndex)) {
							val fieldInfo = helper.fieldsInfo(fieldIndex);
							fieldInfo.oDefaultValue match {
								case Some(defaultValue) =>
									ctorArgs(fieldIndex) = defaultValue
								case None =>
									cursor.miss(s"""Missing required field "${fieldInfo.name}" when trying to parse a "${helper.fullName}""")
							}
						}
						fieldIndex += 1;
					}
					if (cursor.ok) {
						return helper.createProduct(ArraySeq.unsafeWrapArray(ctorArgs))
					}
				} else {
					cursor.fail(s"Invalid syntax for an object while parsing a ${helper.fullName}");
				}
			} else {
				cursor.miss();
			}
		} else {
			cursor.miss(s"A '{' expected but the end of the content was reached when trying to parse a ${helper.fullName}")
		}
		ignored[P]
	}
}

