package lector

import scala.collection.mutable.Builder

object LectorColeccion {
	trait Guia[C, E] {
		def interpretadorElem: Interpretador[E];
		def coleccionBuilder: Builder[E, C]
		val nulableE: Interpretador.Nulable[E];
		val nulableC: Interpretador.Nulable[C];
	}
}

import LectorColeccion._

class LectorColeccion[C <: AnyRef, E](guia: Guia[C, E]) extends LectorJson[C] {

	implicit def nulableE: Interpretador.Nulable[E] = guia.nulableE;
	implicit def nulableC: Interpretador.Nulable[C] = guia.nulableC;

	private def coleccion: Interpretador[C] =
		skipSpaces ~> '[' ~> (skipSpaces ~> guia.interpretadorElem).repSepGen(coma, guia.coleccionBuilder) <~ ']'

	override def interpretar(puntero: Interpretador.Puntero): C = coleccion.interpretar(puntero)
}
