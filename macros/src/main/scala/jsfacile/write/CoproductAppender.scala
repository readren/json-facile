package jsfacile.write

import jsfacile.macros.CoproductAppenderMacro.CahProductInfo
import jsfacile.macros.{CoproductAppenderMacro, CoproductUpperBound}
import jsfacile.util.BinarySearch
import jsfacile.write.CoproductAppender.UnexpectedProductTypeException

object CoproductAppender {

	class UnexpectedProductTypeException(coproductName: String, productName: String) extends RuntimeException(s"coproductName: $coproductName, productName: $productName")
}

class CoproductAppender[C <: CoproductUpperBound](fullName: String, productsInfo: Array[CahProductInfo[C]]) extends Appender[C] {

	override def append(record: Record, product: C): Record = {

		val productName = product.getClass.getName;
		val productInfo = BinarySearch.find[CahProductInfo[C]](productsInfo) { p =>
			CoproductAppenderMacro.productNameComparator.compare(p.name, productName)
		}
		if(productInfo != null) {
			productInfo.appender.append(record, product)
		} else {
			throw new UnexpectedProductTypeException(fullName, productName)
		}
	}
}
