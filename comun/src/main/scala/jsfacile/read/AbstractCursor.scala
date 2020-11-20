package jsfacile.read

import jsfacile.read.Parser.Elem

abstract class AbstractCursor extends Cursor {

	protected var isMissed: Boolean = false;
	protected var isFailed: Boolean = false;
	protected var lastFailure: AnyRef = _;
	protected var lastMissCause: String = _;

	@inline override def ok: Boolean = !isMissed && !isFailed;

	@inline override def have: Boolean = ok && isPointing

	@inline override def missed: Boolean = isMissed && !isFailed;

	@inline override def failed: Boolean = isFailed;

	@inline override def miss(): Unit = {
		this.isMissed = true
		this.lastMissCause = null;
	};
	@inline override def miss(cause: String): Unit = {
		this.isMissed = true;
		this.lastMissCause = cause;
	};

	override def missCause: String = lastMissCause;

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

	override def consumeChar(char: Char): Boolean = {
		if (have && pointedElem == char) {
			this.advance(1)
			have;
		} else {
			false
		}
	}
	override def consumeCharIf(predicate: Char => Boolean): Boolean = {
		if (have && predicate(pointedElem)) {
			this.advance( 1)
			have;
		} else {
			false
		}
	}

	override def consumeWhitespaces(): Boolean = {
		while (have && pointedElem.isWhitespace) {
			this.advance( 1)
		}
		have
	}

	override def consumeWhile(predicate: Elem => Boolean): Boolean = {
		while (have && predicate(pointedElem)) {
			this.advance( 1)
		}
		have
	}
}
