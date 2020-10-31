package jsfacile.read

import jsfacile.read.Parser.{Elem, Pos, Cursor}

/** A [[Parser.Cursor]] whose content is all contained in a single [[String]]. */
class CursorStr(content: String) extends Cursor {

	protected var cursorPos: Int = 0;
	protected var isMissed: Boolean = false;
	protected var isFailed: Boolean = false;
	protected var lastFailure: AnyRef = _;

	override def pos: Pos = cursorPos;

	@inline override def ok: Boolean = !isMissed && !isFailed;

	@inline override def have: Boolean = {
		ok && cursorPos < content.length
	}

	override def atEnd: Boolean = {
		cursorPos == content.length
	}

	override def missed: Boolean = isMissed && !isFailed;

	override def failed: Boolean = isFailed;

	@inline override def pointedElem: Elem = {
		content.charAt(cursorPos)
	};

	override def comes(expected: String): Boolean = {
		//		assert(ok)
		val el = expected.length
		if (content.regionMatches(cursorPos, expected, 0, el)) {
			this.cursorPos += el;
			true
		} else {
			false
		}

	};

	@inline override def advance(cantPasos: Int): Boolean = {
		//		assert(ok)
		this.cursorPos += cantPasos;
		have
	}

	override def miss(): Unit = this.isMissed = true;

	override def fail(cause: AnyRef): Unit = {
		if(!this.isFailed) {
			this.lastFailure = cause
			this.isFailed = true
		};
	};

	override def failureCause: AnyRef =
		if (this.isFailed) this.lastFailure
		else null;

	override def clearMiss(): Unit =
		this.isMissed = false;

	override def repair(): Unit = {
		this.isMissed = false
		this.isFailed = false;
	}


	override def attempt[@specialized(Int, Char) X](block: () => X): X = {
		val startingPos = cursorPos;
		val x = block();
		// si está la marca de fracaso puesta y no la de falla, recuperar posición.
		if (isMissed && !isFailed) {
			this.cursorPos = startingPos
		}
		x
	}

	override def stringConsumedBy(consumer: () => Unit): String = {
		val startingPos = cursorPos;
		consumer();
		if (isFailed) {
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

	override def consumeChar(char: Char): Boolean = {
		if (have && pointedElem == char) {
			this.cursorPos += 1;
			have;
		} else {
			false
		}
	}
	override def consumeCharIf(predicate: Char => Boolean): Boolean = {
		if (have && predicate(pointedElem)) {
			this.cursorPos += 1;
			have;
		} else {
			false
		}
	}

	override def consumeWhitespaces(): Boolean = {
		while (have && pointedElem.isWhitespace) {
			this.cursorPos += 1
		}
		have
	}

	override def consumeWhile(predicate: Elem => Boolean): Boolean = {
		while (have && predicate(pointedElem)) {
			this.cursorPos += 1
		}
		have
	}
}
