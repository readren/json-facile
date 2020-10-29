package jsfacile

import scala.collection.mutable
import scala.language.experimental.macros

import jsfacile.joint.Coproduct
import jsfacile.macros.{CoproductAppenderHelper, ProductAppender}

/** The implicit defined in this package object should NOT be imported in order to have less precedence than the implicit defined in the [[write.api]] package object, which should be imported.
 * The compiler finds the implicit defined here when it searches for instances of the [[Appender]] trait because it belongs to this package object. */
package object write {


	////////////////////////////////////////////////////////////////////////////
	//// Json appenders for products and coproducts (sealed abstract types) ////

	implicit def jaProduct[P <: ProductAppender.UpperBound]: Appender[P] = macro ProductAppender.materializeImpl[P]


	private val jaCoproductCache = mutable.WeakHashMap.empty[String, CoproductAppender[_ <: Coproduct]]

	implicit def jaCoproduct[C <: Coproduct](implicit helper: CoproductAppenderHelper[C]): CoproductAppender[C] = {
		jaCoproductCache.getOrElseUpdate(helper.fullName, new CoproductAppender(helper)).asInstanceOf[CoproductAppender[C]]
	};

}
