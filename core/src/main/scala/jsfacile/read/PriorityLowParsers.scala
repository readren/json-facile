package jsfacile.read

import jsfacile.macros.{CoproductParserHelper, CoproductUpperBound, ProductParserHelper, ProductUpperBound, SingletonParserHelper}

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.read]] package object implements it; and said package is where the [[Parser]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Parser]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityLowParsers {

	///////////////////////////////////////////////////////////
	//// Json parser for singleton classes (scala object)  ////

	implicit def jpSingleton[S](implicit helper: SingletonParserHelper[S]): Parser[S] = { cursor =>
		val ok = Skip.jsObject(cursor);
		if (!ok) {
			cursor.miss(s"A json empty object was expected while parsing the singleton object ${helper.instance.getClass.getName}")
		}
		helper.instance
	}

	/////////////////////////////////////////////////////////
	//// Json parser for concrete non singleton classes  ////

	implicit def jpProduct[P <: ProductUpperBound](implicit helper: ProductParserHelper[P]): ProductParser[P] =  new ProductParser[P](helper);

	///////////////////////////////////////////////////////////////////
	//// Json parser for sealed trait and sealed abstract classes  ////

	implicit def jpCoproduct[C <: CoproductUpperBound](implicit helper: CoproductParserHelper[C]): CoproductParser[C] = new CoproductParser[C](helper);

}
