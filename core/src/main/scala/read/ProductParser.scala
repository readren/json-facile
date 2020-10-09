package read

import scala.collection.mutable

import read.ProductParserHelper.FieldInfo


object ProductParser {
	trait Field[V] { def name: String }
	private case class DefinedField[@specialized V](name: String, value: V) extends Field[V];
	private object UndefinedField extends Field[Nothing] {
		override def name: String = null.asInstanceOf[String]
	}
}

class ProductParser[T <: AnyRef](implicit helper: ProductParserHelper[T]) extends Parser[T] {
	import ProductParser._
	import Parser._
	import JsonParsers._

	assert(helper != null)

	private val fieldParser: Parser[Field[_]] = {
		string <~ skipSpaces <~ colon <~ skipSpaces >> { nombre =>
			helper.fieldsInfo.get(nombre) match {
				case Some(FieldInfo(fieldValueParser, _, _)) =>
					fieldValueParser ^^ { DefinedField(nombre, _) }

				case None =>
					skipJsValue ^^^ UndefinedField

			}
		}
	}

	private val objectParser: Parser[T] = '{' ~> skipSpaces ~> (fieldParser <~ skipSpaces).rep1SepGen(coma ~> skipSpaces, () => List.newBuilder) <~ '}' ^^ { campos =>
		val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder; // TODO change the sequence implementation to one that don't box the values, or implement one myself.
		for {(fieldName, fieldInfo) <- helper.fieldsInfo} {
			campos.find(fieldName == _.name) match {
				case Some(campo) =>
					argsBuilder.addOne(campo.asInstanceOf[DefinedField[_]].value)

				case None if fieldInfo.oDefaultValue.isDefined =>
					argsBuilder.addOne(fieldInfo.oDefaultValue.get)

				case _ => throw new MissingFieldException(helper.className, fieldName)
			}
		}
		helper.createProduct(argsBuilder.result());
	}

	override def parse(cursor: Cursor): T = objectParser.parse(cursor)
}

