package jsfacile

import jsfacile.macros.{EnumParserMacro, NothingMacros}


/** It is not necessary to import any implicit defined in this package object. The compiler finds them anyway because the [[jsfacile.read.Parser]] trait is defined in the same package. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[jsfacile.read.Parser]] accesible without prefix (imported or declared in the block scope). */
package object read extends PriorityMediumParsers with BasicParsers {

	implicit def jpNothing: Parser[Nothing] = macro NothingMacros.materializeNothingParserImpl

	//////////////////////////////////////
	//// Json parsers for basic types ////

	implicit def jpEnumeration[E <: scala.Enumeration]: Parser[E#Value] = macro EnumParserMacro.materializeImpl[E]


}
