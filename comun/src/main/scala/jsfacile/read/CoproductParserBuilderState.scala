package jsfacile.read

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import jsfacile.joint.namedOrdering
import jsfacile.read.CoproductParser.{CpConsideredField, CpFieldInfo, CpProductInfo}

/** Keeps the state of generation of the code [[Tree]] that builds a [[CoproductParser]] for the specified abstract data type `C`.
 * Caution: This class is not thread safe. Don't share an instance between concurrent threads. */
class CoproductParserBuilderState[C] {

	val productFieldsBuilder: mutable.ArrayBuffer[CpFieldInfo] = ArrayBuffer.empty;
	def addProductField(field: CpFieldInfo): Unit = productFieldsBuilder.addOne(field);
	def productFields: Array[CpFieldInfo] = productFieldsBuilder.sortInPlace()(namedOrdering[CpFieldInfo]).toArray;

	val consideredFieldsBuilder: mutable.ArrayBuffer[CpConsideredField] = ArrayBuffer.empty;
	def addConsideredField(consideredField: CpConsideredField): Unit = consideredFieldsBuilder.addOne(consideredField);
	def consideredFields: Array[CpConsideredField] = consideredFieldsBuilder.sortInPlace()(namedOrdering).toArray;

	val productsInfoBuilder: mutable.ArrayBuffer[CpProductInfo[C]] = ArrayBuffer.empty;
	def addProduct(product: CpProductInfo[C]): Unit = productsInfoBuilder.addOne(product)
	def productsInfo: Array[CpProductInfo[C]] = productsInfoBuilder.sortInPlace()(namedOrdering).toArray;
}
