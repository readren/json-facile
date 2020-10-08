package lector

import scala.collection.mutable.Builder

object LectorColeccion {
	trait Guia[C, E] {
		def interpretadorElem: Interpretador[E];
		def coleccionBuilder: Builder[E, C]
		val ignoraE: Interpretador.Ignora[E];
		val ignoraC: Interpretador.Ignora[C];
	}
}

import LectorColeccion._

class LectorColeccion[C <: AnyRef, E](guia: Guia[C, E]) extends Interpretador[C] {
	import LectoresJson._

	implicit def ignoraE: Interpretador.Ignora[E] = guia.ignoraE;
	implicit def ignoraC: Interpretador.Ignora[C] = guia.ignoraC;

	private def coleccion: Interpretador[C] =
		skipSpaces ~> '[' ~> (skipSpaces ~> guia.interpretadorElem).repSepGen(coma, guia.coleccionBuilder) <~ ']'

	override def interpretar(puntero: Interpretador.Puntero): C = coleccion.interpretar(puntero)
}
