package read

import scala.collection.mutable

import read.CoproductParserHelper.{Coproduct, FieldInfo, ProductInfo, ProductName}
import read.SyntaxParsers.{string, _}

object CoproductParser {
	private sealed trait Field[V] {
		def name: String
	}
	private case class DefinedField[@specialized V](name: String, value: V) extends Field[V];
	private object UndefinedField extends Field[Nothing] {
		override def name: String = null.asInstanceOf[String]
	}

	private implicit def ignoredCoproduct[C <: Coproduct]: Parser.Ignore[C] = IgnoreCoproduct.asInstanceOf[Parser.Ignore[C]]
	private object IgnoreCoproduct extends Parser.Ignore[Null] {
		override def ignored: Null = null;
	}

	private val fieldPrefix: Parser[String] = string <~ skipSpaces <~ colon <~ skipSpaces

	implicit def jpCoproduct[T <: Coproduct](implicit helper: CoproductParserHelper[T]): Parser[T] = new CoproductParser[T](helper)
}


class CoproductParser[C <: Coproduct](helper: CoproductParserHelper[C]) extends Parser[C] {
	import CoproductParser._
	import Parser._

	assert(helper != null)

	private class Gestor(val viableProducts: mutable.Map[ProductName, ProductInfo[_ <: C]])

	private def fieldParser(gestor: Gestor): Parser[Field[_]] = fieldPrefix >> { fieldName =>
		if (fieldName == helper.discriminator) {
			string ^^ { productName =>
				gestor.viableProducts.filterInPlace { case (pn, _) => pn == productName }
				UndefinedField
			}
		} else {
			helper.fieldsInfo.get(fieldName) match {

				case Some(FieldInfo(fieldValueParser)) =>
					gestor.viableProducts.filterInPlace { case (_, productInfo) => productInfo.fieldsNames.contains(fieldName) }

					fieldValueParser ^^ {DefinedField(fieldName, _)}

				case None =>
					skipJsValue ^^^ UndefinedField
			}
		}
	}

	private val productParser: Parser[C] = {
		val gestor = new Gestor(mutable.Map.from(helper.productsInfo));

		'{' ~> skipSpaces ~> (fieldParser(gestor) <~ skipSpaces).rep1SepGen(coma ~> skipSpaces, () => List.newBuilder) <~ '}' >> { fields =>
			val viableProductsIterator = gestor.viableProducts.iterator;
			if (viableProductsIterator.hasNext) {
				val (chosenProductName, chosenProductInfo) = viableProductsIterator.next();
				if (viableProductsIterator.hasNext) {
					fail[C] // more than one product (viableProducts) has the fields contained in the JsonObject
				} else {
					val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder; // TODO change the sequence implementation to one that don't box the values, or implement one myself.
					for {(fieldName, oDefaultValue) <- chosenProductInfo.fieldsNames} {
						fields.find(fieldName == _.name) match {

							case Some(field) =>
								argsBuilder.addOne(field.asInstanceOf[DefinedField[_]].value)

							case None if oDefaultValue.isDefined =>
								argsBuilder.addOne(oDefaultValue.get);

							case _ => throw new MissingFieldException(chosenProductName, fieldName)
						}
					}
					hit(chosenProductInfo.constructor(argsBuilder.result()));
				}
			} else {
				fail[C] // there are no product belonging to this coproduct that has all the fields contained in the pointed JsonObject that are defined in at least one of said products.
			}
		}
	}

	override def parse(cursor: Cursor): C = productParser.parse(cursor)
}
