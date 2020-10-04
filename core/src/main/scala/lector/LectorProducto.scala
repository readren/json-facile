package lector

import scala.collection.mutable
import scala.language.experimental.macros

import lector.GuiaLectorProducto.InfoCampo
import lector.LectorJson.MissingFieldException

class LectorProducto[T <: AnyRef](implicit guia: GuiaLectorProducto[T]) extends LectorJson[T] {
	import Interpretador._

	private case class Campo[V](nombre: String, eValor: Either[Pos, V]);


	private def campo: Interpretador[Campo[_]] = skipSpaces ~> string <~ colon >> { nombre =>
		guia.infoCampos.get(nombre) match {
			case Some(InfoCampo(interpretadorValorCampo, _)) =>
				interpretadorValorCampo ^^ { valor => Campo(nombre, Right(valor)) }

			case None =>
				skipJsValue ^^ { pos =>  Campo(nombre, Left(pos)) }

		}
	}

	private def objeto: Interpretador[T] = '{' ~> campo.rep1SepGen(coma, () => List.newBuilder) <~ skipSpaces <~ '}' ^^ { campos =>
		val argsBuilder: mutable.Builder[AnyRef, List[AnyRef]] = List.newBuilder;
		for ((nombreCampo, infoCampo) <- guia.infoCampos) {
			campos.find(_.nombre == nombreCampo) match {
				case Some(Campo(_, Right(valorInterpretado))) =>
					argsBuilder.addOne(valorInterpretado)

				case None if infoCampo.oValorPorOmision.isDefined =>
					argsBuilder.addOne(infoCampo.oValorPorOmision.get)

				case _ => throw new MissingFieldException(guia.className, nombreCampo)
			}
		}
		guia.crear(argsBuilder.result());
	}

	override def interpretar(puntero: Puntero): T = objeto.interpretar(puntero)
}
