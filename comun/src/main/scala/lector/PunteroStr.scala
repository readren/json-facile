package lector

import lector.Interpretador.{Elem, Pos, Puntero}

object PunteroStr {
	private val EN_FALLA_MASK: Long = 0x10_00000000L;
}

import lector.PunteroStr._

class PunteroStr(contenido: String) extends Puntero {
	private var posYFalla: Long = 0

	override def pos: Int = posYFalla.toInt

	override def elemApuntado: Elem = contenido.codePointAt(pos);

	override def viene(esperado: String): Boolean = {
		((posYFalla & EN_FALLA_MASK)==0) && contenido.regionMatches(pos, esperado, 0, esperado.length)
	};

	override def avanzar(cantPasos: Int): Unit = {
		val suma = contenido.offsetByCodePoints(pos, cantPasos);
		posYFalla = suma;
	}
	override def retroceder(): Unit = {
		val suma = contenido.offsetByCodePoints(pos, -1);
		posYFalla = suma;
	}

	override def enFin: Boolean = pos == contenido.length

	override def tomarFotoEstado: Long = posYFalla

	override def revertir(estado: Long): this.type = {
		this.posYFalla = estado
		this
	}
	override def ponerEnFalla(enFallaNuevo: Boolean): Unit = {
		if (enFallaNuevo ^ this.enFalla) posYFalla ^= EN_FALLA_MASK;
	}
	override def enFalla: Boolean = (pos & EN_FALLA_MASK) > 0;

}
