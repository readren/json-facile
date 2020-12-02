package jsfacile.read

import jsfacile.joint.{CoproductUpperBound, ProductUpperBound}
import jsfacile.macros.{CoproductParserMacro, ProductParserMacro, SingletonParserMacro}

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.read]] package object implements it; and said package is where the [[Parser]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Parser]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityLowParsers {

	///////////////////////////////////////////////////////////
	//// Json parser for singleton classes (scala object)  ////

	implicit def jpSingleton[S]: SingletonParser[S] = macro SingletonParserMacro.materializeImpl[S];

	/////////////////////////////////////////////////////////
	//// Json parser for concrete non singleton classes  ////

	implicit def jpProduct[P <: ProductUpperBound]: Parser[P] = macro ProductParserMacro.materializeImpl[P];

	///////////////////////////////////////////////////////////////////
	//// Json parser for sealed trait and sealed abstract classes  ////

	implicit def jpCoproduct[C <: CoproductUpperBound]: Parser[C] = macro CoproductParserMacro.materializeImpl[C];

}
