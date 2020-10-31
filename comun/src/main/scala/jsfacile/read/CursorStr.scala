package jsfacile.read

import jsfacile.read.Parser.{Elem, Pos, Cursor}

/** A [[Parser.Cursor]] whose content is all contained in a single [[String]]. */
class CursorStr(content: String) extends AbstractCursor {

	protected var cursorPos: Int = 0;

	override def pos: Pos = cursorPos;

	override def isPointing: Boolean = cursorPos < content.length && cursorPos >= 0;


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

	@inline override def advance(steps: Int): Boolean = {
		//		assert(ok && cantPasos >= 0)
		this.cursorPos += steps;
		cursorPos < content.length;
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

	override def stringConsumedBy(consumer: Cursor => Unit): String = {
		val startingPos = cursorPos;
		consumer(this);
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
}
