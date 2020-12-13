package jsfacile.read

import jsfacile.macros.CustomParserMacro

/** A builder of a [[CoproductParser]] for the specified abstract data type `C`.
 * `C` may be non sealed.
 * Caution 1: This class is not thread safe. Don't share an instance between concurrent threads.
 * Caution 2: It is allowed to use at most one instance of this class per type of the `C` parameter.   */
class CoproductParserBuilder[C] {

	val state: CoproductParserBuilderState[C] = new CoproductParserBuilderState[C]

	def add[P]: Unit = macro CustomParserMacro.addCaseImpl[C, P];

	def seal(discriminatorFieldName: String): Parser[C] = macro CustomParserMacro.sealImpl[C];

}
