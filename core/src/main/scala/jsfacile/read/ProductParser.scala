package jsfacile.read

import scala.collection.mutable

import jsfacile.macros.ProductParserHelper
import jsfacile.macros.ProductParserHelper.PphFieldInfo
import jsfacile.read.Parser._

object ProductParser {
	private sealed trait Field[+V] {
		def name: String
	}
	private case class DefinedField[@specialized +V](name: String, value: V) extends Field[V];
	private object UndefinedField extends Field[Nothing] {
		override def name: String = null.asInstanceOf[String]
	}

	private implicit def ignoredProduct[T <: Product]: Parser.Ignore[T] = IgnoreProduct.asInstanceOf[Parser.Ignore[T]]
	private object IgnoreProduct extends Parser.Ignore[Null] {
		override def ignored: Null = null;
	}
}

class ProductParser[P <: Product](helper: ProductParserHelper[P]) extends Parser[P] {
	import ProductParser._
	import SyntaxParsers._

	assert(helper != null); // Fails here when the macro expansion of ProductParserHelper fails for some reason. Usually because a compilation error of the expanded code. To find the place in the log search the string "<empty>"

	private val fieldParser: Parser[Field[Any]] = {
		string <~ skipSpaces <~ colon <~ skipSpaces >> { fieldName =>
			helper.fieldsInfo.get(fieldName) match {

				case Some(PphFieldInfo(fieldValueParser, _)) =>
					fieldValueParser.map(DefinedField(fieldName, _))

				case None =>
					skipJsValue.^^^(UndefinedField)
			}
		}
	}

	private val objectParser: Parser[P] = '{' ~> skipSpaces ~> (fieldParser <~ skipSpaces).rep1SepGen(coma ~> skipSpaces, () => List.newBuilder) <~ '}' ^^ { fields =>
		val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder; // TODO change the sequence implementation to one that don't box the values, or implement one myself.
		for {(fieldName, fieldInfo) <- helper.fieldsInfo} {
			fields.find(fieldName == _.name) match {

				case Some(field) =>
					argsBuilder.addOne(field.asInstanceOf[DefinedField[Any]].value)

				case None if fieldInfo.oDefaultValue.isDefined =>
					argsBuilder.addOne(fieldInfo.oDefaultValue.get)

				case _ => throw new MissingFieldException(helper.fullName, fieldName)
			}
		}
		helper.createProduct(argsBuilder.result());
	}

	override def parse(cursor: Cursor): P = {
		val p = objectParser.parse(cursor);
		if (cursor.missed) {
			cursor.fail(s"Invalid json object format found while parsing an instance of ${helper.fullName}")
		}
		p
	}
}

