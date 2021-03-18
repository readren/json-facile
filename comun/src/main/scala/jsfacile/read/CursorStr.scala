package jsfacile.read

import jsfacile.read.Parser.{Elem, Pos}

/** A [[Cursor]] whose content is all contained in a single array of [[Char]]s.
 *
 * This type of [[Cursor]]s has the particularity that the [[pos]] method returns the number of consumed chars. */
class CursorStr(content: Array[Char]) extends AbstractCursor {

	def this(content: String) = this(content.toCharArray);

	def this(content: CharSequence) = this(content.toString.toCharArray)

	protected var cursorPos: Int = 0;

	@inline override def pos: Pos = cursorPos;

	@inline override def isPointing: Boolean = cursorPos < content.length && cursorPos >= 0;


	@inline override def pointedElem: Elem = {
		content(cursorPos)
	};

	override def comes(expected: String): Boolean = {
		//		assert(ok)
		val el = expected.length
		var i = 0;
		var j = this.cursorPos;
		while (i < el && j < content.length && content(j) == expected.charAt(i)) {
			i += 1;
			j += 1;
		}

		if (i == el) {
			this.cursorPos += el;
			true
		} else {
			false
		}
	}

	@inline override def advance(steps: Int): Boolean = {
		//		assert(ok)
		this.cursorPos += steps;
		cursorPos < content.length;
	}

	override def attempt[@specialized(Int, Char) X](block: Cursor => X): X = {
		val startingPos = cursorPos;
		val x = block(this);
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
				new String(content, startingPos, cursorPos - startingPos)
			}
		}
	}

	override def posOfNextEscapeOrClosingQuote: Pos = {
		var cp = this.cursorPos;
		if (this.content(cp) == '"') {
			do cp += 1
			while (cp < this.content.length && {val pe = this.content(cp); pe != '"' && pe != '\\'});
			cp
		} else 0
	}

	override def consumeStringUntil(pos: Pos): String = {
		val start = this.cursorPos + 1;
		val s = new String(this.content, start, pos - start);
		this.cursorPos = pos + 1;
		s
	}
	override def consumeStringTo(pos: Pos): String = {
		val s = this.consumeStringUntil(pos);
		this.cursorPos -= 1;
		s
	}
}
