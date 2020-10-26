import read.CoproductParserHelper.Coproduct

/** The implicit defined in this package object should NOT be imported in order to have less precedence than the implicit defined in the [[write.api]] package object, which should be imported.
 * The compiler finds the implicit defined here when it searches for instances of the [[Appender]] trait because it belongs to this package object. */
package object write {


	////////////////////////////////////////////////////////////////////////////
	//// Json appenders for products and coproducts (sealed abstract types) ////

	implicit def jaProduct[P <: ProductAppender.UpperBound]: Appender[P] = macro ProductAppender.materializeImpl[P]

	implicit def jaCoproduct[C <: Coproduct](implicit helper: CoproductAppenderHelper[C]): Appender[C] = new CoproductAppender(helper);

}
