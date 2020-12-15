package jsfacile.write

import jsfacile.macros.macrosEntrance

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.write]] package object implements it; and said package is where the [[Appender]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[jsfacile.write.Appender]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityLowAppenders {

	///////////////////////////////////////////
	//// JSON appender for custom classes ////

	implicit def jaCustom[P]: Appender[P] = macro macrosEntrance.materializeAppenderImpl[P]

}
