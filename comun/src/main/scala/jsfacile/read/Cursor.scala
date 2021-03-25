package jsfacile.read

import jsfacile.read.Parser.{Elem, Pos}
import jsfacile.util.CharPredicate

/** The low-level API on which [[Parser]] implementations are built.
 * The [[Parser.parse]] method receives an instance of this class from which it extracts the input elements and to which it mutates to communicate the parsing progress to the next parser.
 * This trait models the requirements that said cursor should obey.
 * @define posCaution CAUTION: the value returned by this method is not necessarily the number of consumed chars. It may depend on the way the content is stored by the implementation. It is only meaningful for this instance, and is intended to be used only as argument for methods of this instance (like [[consumeStringTo]]).
 * */
trait Cursor {
	/** Current cursor position.
	 *
	 * $posCaution */
	@inline def pos: Pos;
	/** This cursor missed and failed flags are not set. */
	@inline def ok: Boolean;
	/** This [[Cursor]] is [[ok]] and also is pointing to an element of the content (not at the end). */
	@inline def have: Boolean
	/** Is true when this [[Cursor]] is pointing to an element of the content and, therefore, the [[pointedElem]] has a valid value. This state changes to false when all the elements are consumed. */
	def isPointing: Boolean;
	/** Is true when this [[Cursor]] missed flag is set and the failure flag is not. */
	def missed: Boolean;
	/** The element being pointed by this [[Cursor]]. Assumes this cursor is pointing to an element of the content. */
	@inline def pointedElem: Elem
	/** If the received [[java.lang.String]] matches the content subsequence starting at the [[pointedElem]], then said subsequence is consumed (the cursor advances the length of the received [[java.lang.String]]) and returns true. Otherwise nothing is changed and returns false. */
	def comes(expected: String): Boolean
	/** Increments the position.
	 *
	 * @param steps number of [[Char]]s to advance. Should be a positive number.
	 *
	 * Note that advancing X chars does not necessarily increases the [[pos]] in the same amount.
	 * @see [[pos]].
	 * @return true if after the advance this cursor is pointing to an element of the content (same as [[isPointing]]). */
	def advance(steps: Int = 1): Boolean
	/** Sets the missed flag. Parsers set this flag when they miss. The [[Parser.orElse]] operator clears this flag before applying the second parser, when the first has missed. */
	def miss(): Unit
	/** Sets the missed flag and informs the cause. Parsers set this flag when they miss. The [[Parser.orElse]] operator clears this flag before applying the second parser, when the first has missed. */
	def miss(cause: String): Unit;
	/** Get the miss cause or null if no cause was given in the last miss. */
	def missCause: String;
	/** If already failed does nothing, else sets the failed flag and memorizes the cause. Failures propagates trough all normal parsers until a [[Parser.recover]] or [[Parser.recoverWith]] is reached. */
	def fail(cause: AnyRef): Unit
	/** The cause of the last failure or null if the failing flag is not set. */
	def failureCause: AnyRef
	/** Is true when this [[Cursor]] failed flag is set */
	def failed: Boolean;
	/** Clears the missed flag. Note that the [[missed]] method will continue giving false if the [[failed]] flag is set. */
	def clearMiss(): Unit
	/** Clears both, the missed and failed flags. */
	def repair(): Unit

	/** The implementation should apply the received function to this [[Cursor]] instance and, if after that this [[Cursor]] is missed but not failed, should recover the position it had before the block had been executed.
	 *
	 * @param block procedure which may and usually do mutate this cursor.
	 * @return the result given by the block */
	def attempt[@specialized(Int, Char) X](block: Cursor => X): X

	/** The implementation should execute the received block and, if after that this [[Cursor]]:
	 *  - is [[ok]], should return a [[java.lang.String]] containing the code points consumed by the block;
	 *  - is missed but not failed, should recover the position it had before the block had been executed and return `null`;
	 *  - is failed, should return `null`.
	 *
	 * @return a [[java.lang.String]] containing the elements consumed by the `consumer` if the cursor is [[ok]] after the `consumer` block execution. `null` otherwise. The consumer may and usually does mutate this cursor. */
	def stringConsumedBy(consumer: Cursor => Unit): String

	/** If the cursor [[have]] and the pointed element equals the received char, advances to next position and returns [[have]]. Else does nothing and returns false.
	 *
	 * @return true if and only if the element was consumed and after that the cursor is pointing to an element of the content (implies that both the missed and failed flags are false because otherwise the element won't be consumed). In other word, returns [[have]] if the element was consumed, false otherwise. */
	def consumeChar(char: Char): Boolean;

	/** If the cursor [[have]] and the pointed element satisfies the predicate, advances to next position and returns [[have]]. Else does nothing and returns false.
	 *
	 * @return true if and only if the element was consumed and after that the cursor is pointing to an element of the content (implies that both the missed and failed flags are false because otherwise the element won't be consumed). In other word, returns [[have]] if the element was consumed, false otherwise. */
	def consumeCharIf(predicate: CharPredicate): Boolean;

	/** If the cursor [[have]] and the pointed element is a digit, advances to next position and returns [[have]]. Else does nothing and returns false.
	 *
	 * VERY IMPORTANT: this method should be extremely efficient. It is used to parse [[scala.Float]], [[scala.Double]], [[scala.BigInt]], and [[scala.BigDecimal]].
	 *
	 * @return true if and only if the element was consumed and after that the cursor is pointing to an element of the content (implies that both the missed and failed flags are false because otherwise the element won't be consumed). In other word, returns [[have]] if the element was consumed, false otherwise. */
	def consumeCharIfDigit(): Boolean

	/** If the cursor [[have]] and the pointed element is either of the received chars, advances to next position and returns [[have]]. Else does nothing and returns false.
	 *
	 * @return true if and only if the element was consumed and after that the cursor is pointing to an element of the content (implies that both the missed and failed flags are false because otherwise the element won't be consumed). In other word, returns [[have]] if the element was consumed, false otherwise. */
	def consumeCharIfEither(a: Char, b: Char): Boolean;

	/** If the cursor [[have]] and the pointed element is a whitespace char, advances positions until the first non whitespace char after it. Else sets the missed flag.
	 *
	 * Equivalent to {{{consumeWhile(_.isWhitespace)}}}
	 *
	 * @return true if the cursor is pointing to an element of the content independently if whitespaces were consumed or not. In other words, returns the new state of [[have]] */
	def consumeWhitespaces(): Boolean

	/** Advances the cursor position while it [[have]] and the pointed element satisfies the predicate.
	 *
	 * @return true if the cursor is pointing to an element of the content and both the missed and failed flags are false. In other words, returns the new state of [[have]] */
	def consumeWhile(predicate: CharPredicate): Boolean;

	/** Advances the cursor position while it [[have]] and the pointed element is a digit.
	 *
	 * VERY IMPORTANT: this method should be extremely efficient. It is used to parse [[scala.Float]], [[scala.Double]], [[scala.BigInt]], and [[scala.BigDecimal]]. *
	 *
	 * @return true if the cursor is pointing to an element of the content and both the missed and failed flags are false. In other words, returns the new state of [[have]] */
	def consumeWhileDigit(): Boolean

	/** If the pointed element isn't a quote, returns 0. If it is a quote, gives the position of the first occurrence of either, a quote, a escape, or the content end (length of the content).
	 *
	 * Asumes that the cursor [[isPointing]].
	 *
	 * This method does not mutate this instance.
	 *
	 * VERY IMPORTANT: this method should be extremely efficient. It is used to parse every string, including field names.
	 *
	 * $posCaution*/
	def posOfNextEscapeOrClosingQuote: Pos

	/** Takes the substring of the content that starts at the pointed position (exclusive), and ends at the specified `pos` (exclusive).
	 *
	 * This method always exits with [[Cursor.pos]] equal to the received `pos`.
	 *
	 * Assumes that the specified `pos` is greater than this [[Cursor.pos]] and less than the content length. It is designed to be called after [[posOfNextEscapeOrClosingQuote]].
	 *
	 * VERY IMPORTANT: this method should be extremely efficient. It is used to parse every string, including field names. */
	def consumeStringTo(pos: Pos): String
}
