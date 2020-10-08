package lector

import lector.Interpretador.{Elem, Pos, Puntero}

class PunteroStr(contenido: String) extends Puntero {

	private var cursorPos: Int = 0;
	private var estaFracasado: Boolean = false;
	private var estaFallado: Boolean = false;

	override def pos = cursorPos;

	@inline override def ok: Boolean = !estaFracasado && !estaFallado;

	override def hay: Boolean = {
		assert(ok);
		0 <= cursorPos && cursorPos < contenido.length
	}

	override def enFin: Boolean = {
		assert(ok)
		cursorPos == contenido.length
	}

	override def fracasado: Boolean = estaFracasado;

	override def fallado: Boolean = estaFallado;

	override def elemApuntado: Elem = {
		assert(hay);
		contenido.codePointAt(cursorPos)
	};

	override def viene(esperado: String): Boolean = {
		ok && contenido.regionMatches(cursorPos, esperado, 0, esperado.length);
	};

	override def avanzar(cantPasos: Int): Unit = {
		assert(hay);
		this.cursorPos = contenido.offsetByCodePoints(cursorPos, cantPasos);
	}

	override def fracasar(): Unit = this.estaFracasado = true;

	override def fallar(): Unit = this.estaFallado = true;

	override def reparar(): Unit = {
		this.estaFracasado = false
		this.estaFallado = false;
	}


	override def intentar[@specialized X](bloque: () => X): X = {
		val memoria = cursorPos;
		val x = bloque();
		// si está la marca de fracaso puesta y no la de falla, recuperar posición.
		if (estaFracasado && !estaFallado) {
			this.cursorPos = memoria;
		}
		x
	}
}
