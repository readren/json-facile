package jsfacile.write

import java.util.Comparator

import jsfacile.joint.CoproductUpperBound
import jsfacile.util.BinarySearch
import jsfacile.write.CoproductAppender.{CahProductInfo, UnexpectedProductTypeException, productNameComparator}

object CoproductAppender {

	case class CahProductInfo[P](name: String, appender: Appender[P])

	/** Compares two [[CahProductInfo]] based their names using the [[productNameComparator]] as ordering criteria.. */
	val productInfoComparator: Comparator[CahProductInfo[_]] = { (a, b) => productNameComparator.compare(a.name, b.name) }

	/** Compares two class names based on the length and, if both have the same length, by alphabetic order of the reversed names.
	 * If the second name (`b`) ends with a dollar, or a dollar followed by digits, they are removed before the comparison begins. This is necessary because the runtime class name of module classes have an extra dollar at the end, local classes have a dollar followed by digits, and local object digits surrounded by dollars.
	 * Differences between dots and dollars are ignored if the dot is in the first name (`a`) and the dollar in the second name (`b`). This is necessary because the runtime class name of nested classes use a dollar instead of a dot to separate the container from members.
	 * The names are considered equal if the fragments after the last dot are equal. */
	val productNameComparator: Comparator[String] = { (a, b) =>
		val aLength = a.length;
		var bLength = b.length;
		var bChar: Char = 0;
		var index: Int = 0;

		// Ignore the last segment of `b` if it matches "(\$\d*)+". This is necessary because the runtime class name of: module classes have an extra dollar at the end, local classes have a dollar followed by a number, and local object have a number surrounded by dollars.
		// Optimized versión
		var continue = false;
		do {
			index = bLength - 1;
			continue = false;
			//  find the index of the last non digit character
			while ( {bChar = b.charAt(index); Character.isDigit(bChar)}) {
				index -= 1;
			}
			// if the index of the last non digit character is a dollar, remove it along with the succeeding digits for the comparison.
			if (b.charAt(index) == '$') {
				bLength = index;
				// if something was removed, repeat the process again to support combinations of edge cases. It is not necessary to know all the edge cases if it's known that any dollar or dollar followed by digits at the end are not part of the original class name. So we can remove combinations of them without fear.
				continue = true;
			}
		} while (continue)

		// here starts the comparison
		var diff = aLength - bLength;
		if (diff == 0 && aLength > 0) {
			index = aLength - 1;
			do {
				val aChar = a.charAt(index);
				bChar = b.charAt(index);
				diff = if (aChar == '.' && bChar == '$') {
					0 // Ignore difference between dots and dollars. This assumes that the first name (an) is obtained by the macro, and the second (bn) may be obtained at runtime from the Class object.
				} else {
					aChar - bChar;
				}
				index -= 1;
			} while (diff == 0 && index >= 0 && bChar != '.')
		}
		diff
	}


	class UnexpectedProductTypeException(coproductName: String, productName: String) extends RuntimeException(s"coproductName: $coproductName, productName: $productName")
}

class CoproductAppender[C <: CoproductUpperBound](fullName: String, productsInfo: Array[CahProductInfo[C]]) extends Appender[C] {

	override def append(record: Record, product: C): Record = {

		val productName = product.getClass.getName;
		val productInfo = BinarySearch.find[CahProductInfo[C]](productsInfo) { p =>
			productNameComparator.compare(p.name, productName)
		}
		if(productInfo != null) {
			productInfo.appender.append(record, product)
		} else {
			throw new UnexpectedProductTypeException(fullName, productName)
		}
	}
}
