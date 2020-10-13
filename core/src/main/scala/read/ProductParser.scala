package read

import scala.collection.mutable

import read.PrimitiveParsers._
import read.Parser._
import read.ProductParserHelper.FieldInfo


object ProductParser {
	private sealed trait Field[V] {
		def name: String
	}
	private case class DefinedField[@specialized V](name: String, value: V) extends Field[V];
	private object UndefinedField extends Field[Nothing] {
		override def name: String = null.asInstanceOf[String]
	}

	private implicit def ignoredProduct[T <: Product]: Parser.Ignore[T] = IgnoreProduct.asInstanceOf[Parser.Ignore[T]]
	private object IgnoreProduct extends Parser.Ignore[Null] {
		override def ignored: Null = null;
	}

	implicit def jpProduct[T <: Product](implicit helper: ProductParserHelper[T]): Parser[T] = new ProductParser[T](helper)
}

class ProductParser[T <: Product](helper: ProductParserHelper[T]) extends Parser[T] {
	import ProductParser._
	import SyntaxParsers._

	assert(helper != null)

	private val fieldParser: Parser[Field[_]] = {
		string <~ skipSpaces <~ colon <~ skipSpaces >> { fieldName =>
			helper.fieldsInfo.get(fieldName) match {

				case Some(FieldInfo(fieldValueParser, _)) =>
					fieldValueParser ^^ { DefinedField(fieldName, _) }

				case None =>
					skipJsValue ^^^ UndefinedField
			}
		}
	}

	private val objectParser: Parser[T] = '{' ~> skipSpaces ~> (fieldParser <~ skipSpaces).rep1SepGen(coma ~> skipSpaces, () => List.newBuilder) <~ '}' ^^ { fields =>
		val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder; // TODO change the sequence implementation to one that don't box the values, or implement one myself.
		for {(fieldName, fieldInfo) <- helper.fieldsInfo} {
			fields.find(fieldName == _.name) match {

				case Some(field) =>
					argsBuilder.addOne(field.asInstanceOf[DefinedField[_]].value)

				case None if fieldInfo.oDefaultValue.isDefined =>
					argsBuilder.addOne(fieldInfo.oDefaultValue.get)

				case _ => throw new MissingFieldException(helper.className, fieldName)
			}
		}
		helper.createProduct(argsBuilder.result());
	}

	override def parse(cursor: Cursor): T = objectParser.parse(cursor)
}

