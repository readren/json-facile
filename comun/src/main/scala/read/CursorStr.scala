package read

import read.Parser.{Elem, Pos, Cursor}

/** A [[Parser.Cursor]] whose content is all contained in a single [[String]].*/
class CursorStr(content: String) extends Cursor {

	private var cursorPos: Int = 0;
	private var isMissed: Boolean = false;
	private var isFailed: Boolean = false;
	private var lastFailure: AnyRef = null;

	override def pos: Pos = cursorPos;

	@inline override def ok: Boolean = !isMissed && !isFailed;

	@inline override def have: Boolean = {
		assert(ok)
		cursorPos < content.length
	}

	override def atEnd: Boolean = {
		cursorPos == content.length
	}

	override def missed: Boolean = isMissed && !isFailed;

	override def failed: Boolean = isFailed;

	override def pointedElem: Elem = {
		content.codePointAt(cursorPos)
	};

	override def comes(expected: String): Boolean = {
		assert(ok)
		content.regionMatches(cursorPos, expected, 0, expected.length);
	};

	override def advance(cantPasos: Int): Boolean = {
		assert(ok)
		this.cursorPos = content.offsetByCodePoints(cursorPos, cantPasos);
		have
	}

	override def miss(): Unit = this.isMissed = true;

	override def fail(cause: AnyRef): Unit = {
		this.lastFailure = cause;
		this.isFailed = true
	};

	override def failureCause: AnyRef =
		if(this.isFailed) this.lastFailure
		else null;

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
