package jsfacile.write

import java.util.Comparator

import jsfacile.util.BinarySearch
import jsfacile.write.CoproductAppender.{CahProductInfo, UnexpectedProductTypeException, productNameComparator}

object CoproductAppender {

	case class CahProductInfo[P](name: String, appender: Appender[P])

	/** Compares two [[CahProductInfo]] based their names using the [[productNameComparator]] as ordering criteria.. */
	val productInfoComparator: Comparator[CahProductInfo[_]] = { (a, b) => productNameComparator.compare(a.name, b.name) }

	/** Compares two class names by alphabetic order of the reversed text.
	 * If the second name (`b`) ends with a dollar, or a dollar followed by digits, they are removed before the comparison begins. This is necessary because the runtime class name of module classes have an extra dollar at the end, local classes have a dollar followed by digits, and local object digits surrounded by dollars.
	 * Differences between dots and dollars (or a dollar preceded by ".package") are ignored if the dot is in the first name (`a`) and the dollar in the second name (`b`). This is necessary because the runtime class name of nested classes use a dollar instead of a dot to separate the container from members; and a dollar preceded by ".package" when the nest is a package object.
	 * The names are considered equal if the fragments after the last dot in the second name (b) are equal. */
	val productNameComparator: Comparator[String] = { (a, b) =>
		val aLength = a.length;
		var bLength = b.length;
		var bChar: Char = 0;
		var aIndex: Int = aLength - 1;
		var bIndex: Int = 0;

		// Ignore the last segment of `b` if it matches "(\$\d*)+". This is necessary because the runtime class name of: module classes have an extra dollar at the end, local classes have a dollar followed by a number, and local object have a number surrounded by dollars.
		// Optimized versión
		var continue = false;
		do {
			bIndex = bLength - 1;
			continue = false;
			//  find the index of the last non digit character
			while ( {bChar = b.charAt(bIndex); Character.isDigit(bChar)}) {
				bIndex -= 1;
			}
			// if the index of the last non digit character is a dollar, remove it along with the succeeding digits for the comparison.
			if (b.charAt(bIndex) == '$') {
				bLength = bIndex;
				// if something was removed, repeat the process again to support combinations of edge cases. It is not necessary to know all the edge cases if it's known that any dollar or dollar followed by digits at the end are not part of the original class name. So we can remove combinations of them without fear.
				continue = true;
			}
		} while (continue)

		// here starts the comparison
		var diff = 0;
		if (aLength > 0 && bLength > 0) {
			aIndex = aLength;
			bIndex = bLength;
			do {
				aIndex -= 1;
				bIndex -= 1;
				val aChar = a.charAt(aIndex);
				bChar = b.charAt(bIndex);
				diff = if (aChar == '.' && bChar == '$') { // Ignore difference between a dot and a dollar or a dot and a ".package$". This assumes that the first name (a) is obtained by the macro, and the second (b) may be obtained at runtime from the Class object.
					if (bIndex >= 8 && b.charAt(bIndex - 1) == 'e' && b.charAt(bIndex - 2) == 'g' && b.charAt(bIndex - 3) == 'a'&& b.charAt(bIndex - 4) == 'k'&& b.charAt(bIndex - 5) == 'c'&& b.charAt(bIndex - 6) == 'a' && b.charAt(bIndex - 7) == 'p' && b.charAt(bIndex - 8) == '.') {
						bIndex -= 8;
						bLength -= 8;
					}
					0
				} else {
					aChar - bChar;
				}
			} while (diff == 0 && aIndex > 0 && bIndex > 0 && bChar != '.')
			if(diff == 0) {
				diff = aLength - bLength
			}
		} else {
			diff = aLength - bLength
		}
		diff
	}

	class UnexpectedProductTypeException(coproductName: String, productName: String) extends RuntimeException(s"`$productName` is not a known subtype of `$coproductName`.")
}

class CoproductAppender[C](fullName: String, productsInfo: Array[CahProductInfo[C]]) extends Appender[C] {

	override def append(record: Record, product: C): Record = {

		val productName = product.getClass.getName;
		val productInfo = BinarySearch.find[CahProductInfo[C]](productsInfo) { p =>
			productNameComparator.compare(p.name, productName)
		}
		if(productInfo != null) {
			productInfo.appender.append(record, product)
		} else {
			record.fail(new UnexpectedProductTypeException(fullName, productName))
		}
	}
}
