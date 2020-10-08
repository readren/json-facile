package lector

import scala.collection.mutable

import lector.GuiaLectorProducto.InfoCampo
import lector.LectoresJson.MissingFieldException

object LectorProducto {
	trait Campo[V] { def nombre: String }
	private case class CampoDefinido[V](nombre: String, valor: V) extends Campo[V];
	private object CampoIndefinido extends Campo[Nothing] {
		override def nombre: String = null.asInstanceOf[String]
	}
}

class LectorProducto[T <: AnyRef](implicit guia: GuiaLectorProducto[T]) extends Interpretador[T] {
	import LectorProducto._
	import Interpretador._
	import LectoresJson._

	assert(guia != null)

	private val campo: Interpretador[Campo[_]] = {
		string <~ skipSpaces <~ colon <~ skipSpaces >> { nombre =>
			guia.infoCampos.get(nombre) match {
				case Some(InfoCampo(interpretadorValorCampo, _, _)) =>
					interpretadorValorCampo ^^ { CampoDefinido(nombre, _) }

				case None =>
					skipJsValue ^^^ CampoIndefinido

			}
		}
	}

	private val objeto: Interpretador[T] = '{' ~> skipSpaces ~> (campo <~ skipSpaces).rep1SepGen(coma ~> skipSpaces, () => List.newBuilder) <~ '}' ^^ { campos =>
		val argsBuilder: mutable.Builder[Any, List[Any]] = List.newBuilder;
		for {(nombreCampo, infoCampo) <- guia.infoCampos } {
			campos.find(nombreCampo == _.nombre) match {
				case Some(campo) =>
					argsBuilder.addOne(campo.asInstanceOf[CampoDefinido[_]].valor)

				case None if infoCampo.oValorPorOmision.isDefined =>
					argsBuilder.addOne(infoCampo.oValorPorOmision.get)

				case _ => throw new MissingFieldException(guia.className, nombreCampo)
			}
		}
		guia.crear(argsBuilder.result());
	}

	override def interpretar(puntero: Puntero): T = objeto.interpretar(puntero)
}

