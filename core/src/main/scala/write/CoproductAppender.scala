package write

import read.CoproductParserHelper.Coproduct
import util.BinarySearch
import write.CoproductAppender.UnexpectedProductTypeException
import write.CoproductAppenderHelper.CahProductInfo

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
			throw new UnexpectedProductTypeException(helper.name, productName)
		}
	}
}
