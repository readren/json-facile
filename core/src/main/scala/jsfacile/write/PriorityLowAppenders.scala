package jsfacile.write

import jsfacile.macros.{CoproductAppenderHelper, CoproductUpperBound, ProductAppender, ProductUpperBound}

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.write]] package object implements it; and said package is where the [[Appender]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Appender]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityLowAppenders {

	/////////////////////////////////////////////
	//// JSON appenders for concrete classes ////

	implicit def jaProduct[P <: ProductUpperBound]: Appender[P] = macro ProductAppender.materializeImpl[P]

	///////////////////////////////////////////////////////////////////////
	//// Json appenders for sealed traits and sealed abstract classes  ////

	implicit def jaCoproduct[C <: CoproductUpperBound](implicit helper: CoproductAppenderHelper[C]): CoproductAppender[C] = new CoproductAppender(helper);

}
