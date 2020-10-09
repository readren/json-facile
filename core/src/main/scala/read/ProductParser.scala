package read

import scala.collection.mutable

import read.ProductParserHelper.FieldInfo


object ProductParser {
	trait Field[V] { def name: String }
	private case class DefinedField[V](name: String, valor: V) extends Field[V];
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
		val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder;
		for {(nombreCampo, infoCampo) <- helper.fieldsInfo} {
			campos.find(nombreCampo == _.name) match {
				case Some(campo) =>
					argsBuilder.addOne(campo.asInstanceOf[DefinedField[_]].valor)

				case None if infoCampo.oDefaultValue.isDefined =>
					argsBuilder.addOne(infoCampo.oDefaultValue.get)

				case _ => throw new MissingFieldException(helper.className, nombreCampo)
			}
		}
		helper.createProduct(argsBuilder.result());
	}

	override def parse(puntero: Cursor): T = objectParser.parse(puntero)
}

