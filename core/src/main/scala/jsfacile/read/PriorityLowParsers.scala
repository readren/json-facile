package jsfacile.read

import jsfacile.macros.CustomParserMacro

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.read]] package object implements it; and said package is where the [[Parser]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[jsfacile.read.Parser]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityLowParsers {

	///////////////////////////////////////
	//// Json parser for custom types  ////

	implicit def jpCustom[T]: Parser[T] = macro CustomParserMacro.materializeImpl[T];

}
