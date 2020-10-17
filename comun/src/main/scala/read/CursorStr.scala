package read

import read.Parser.{Elem, Pos, Cursor}

/** A [[Parser.Cursor]] whose content is all contained in a single [[String]].*/
class CursorStr(content: String) extends Cursor {

	private var cursorPos: Int = 0;
	private var isMissed: Boolean = false;
	private var isFailed: Boolean = false;
	private var failureCause: AnyRef = null;

	override def pos: Pos = cursorPos;

	@inline override def ok: Boolean = !isMissed && !isFailed;

	override def have: Boolean = {
		assert(ok);
		0 <= cursorPos && cursorPos < content.length
	}

	override def atEnd: Boolean = {
		assert(ok)
		cursorPos == content.length
	}

	override def missed: Boolean = isMissed && !isFailed;

	override def failed: Boolean = isFailed;

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

	override def miss(): Unit = this.isMissed = true;

	override def fail(cause: AnyRef): Unit = {
		this.failureCause = cause;
		this.isFailed = true
	};

	override def clearMiss(): Unit =
		this.isMissed = false;

	override def repair(): Unit = {
		this.isMissed = false
		this.isFailed = false;
	}


	override def attempt[@specialized(Int) X](block: () => X): X = {
		val startingPos = cursorPos;
		val x = block();
		// si está la marca de fracaso puesta y no la de falla, recuperar posición.
		if (isMissed && !isFailed) {
			this.cursorPos = startingPos
		}
		x
	}

	override def consume(block: () => Unit): String = {
		val startingPos = cursorPos;
		block();
		if(isFailed) {
			null
		} else {
			if (isMissed) {
				this.cursorPos = startingPos;
				null
			} else {
				content.substring(startingPos, cursorPos)
			}
		}

	}
}
