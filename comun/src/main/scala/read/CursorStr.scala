package read

import read.Parser.{Elem, Pos, Cursor}

/** A [[Parser.Cursor]] whose content is all contained in a single [[String]].*/
class CursorStr(content: String) extends Cursor {

	private var cursorPos: Int = 0;
	private var isFrustrated: Boolean = false;
	private var isFailing: Boolean = false;

	override def pos: Pos = cursorPos;

	@inline override def ok: Boolean = !isFrustrated && !isFailing;

	override def have: Boolean = {
		assert(ok);
		0 <= cursorPos && cursorPos < content.length
	}

	override def atEnd: Boolean = {
		assert(ok)
		cursorPos == content.length
	}

	override def missed: Boolean = isFrustrated;

	override def failed: Boolean = isFailing;

	override def pointedElem: Elem = {
		assert(have);
		content.codePointAt(cursorPos)
	};

	override def comes(esperado: String): Boolean = {
		ok && content.regionMatches(cursorPos, esperado, 0, esperado.length);
	};

	override def advance(cantPasos: Int): Unit = {
		assert(have);
		this.cursorPos = content.offsetByCodePoints(cursorPos, cantPasos);
	}

	override def miss(): Unit = this.isFrustrated = true;

	override def fail(): Unit = this.isFailing = true;

	override def repair(): Unit = {
		this.isFrustrated = false
		this.isFailing = false;
	}


	override def attempt[@specialized X](block: () => X): X = {
		val memoria = cursorPos;
		val x = block();
		// si está la marca de fracaso puesta y no la de falla, recuperar posición.
		if (isFrustrated && !isFailing) {
			this.cursorPos = memoria;
		}
		x
	}
}
