package jsfacile.write

import jsfacile.joint.Coproduct
import jsfacile.macros.CoproductAppenderHelper
import jsfacile.macros.CoproductAppenderHelper.CahProductInfo
import jsfacile.util.BinarySearch
import jsfacile.write.CoproductAppender.UnexpectedProductTypeException

object CoproductAppender {

	class UnexpectedProductTypeException(coproductName: String, productName: String) extends RuntimeException(s"coproductName: $coproductName, productName: $productName")
}

class CoproductAppender[C <: Coproduct](helper: CoproductAppenderHelper[C]) extends Appender[C] {

	assert(helper != null)

	override def append(record: Record, product: C): Record = {

		val productName = product.getClass.getName;
		val productInfo = BinarySearch.find[CahProductInfo[C]](helper.productsInfo) { p =>
			CoproductAppenderHelper.productNameComparator.compare(p.name, productName)
		}
		if(productInfo != null) {
			productInfo.appender.append(record, product)
		} else {
			throw new UnexpectedProductTypeException(helper.fullName, productName)
		}
	}
}
