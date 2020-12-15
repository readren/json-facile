package jsfacile.api

import jsfacile.macros.macrosEntrance

/** A builder that constructs instances of [[Parser]][C] and/or [[Appender]][C], where `C` is an abstract data type.
 * `C` may be non sealed.
 * Caution 1: This class is not thread safe. Don't share an instance between concurrent threads.
 * Caution 2: It is allowed to use at most one instance of this class per type of the `C` parameter.   */
class CoproductBuilder[C] {

	def add[P]: Unit = macro macrosEntrance.addCaseImpl[C, P];

	def parser: Parser[C] = macro macrosEntrance.sealParserImpl[C];

	def appender: Appender[C] = macro macrosEntrance.sealAppenderImpl[C]

}
