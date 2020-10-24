import read.CoproductParserHelper.Coproduct

/** The implicit defined in this package object should NOT be imported in order to have less precedence than the implicit defined in the [[read.api]] package object, which should be imported.
 * The compiler finds the implicits defined here when it searches for instances of the [[Parser]] trait because it belongs to this package object. */
package object read {

	implicit val jpString: Parser[String] = PrimitiveParsers.jpString

	/** Interpretador de Int en Json */
	implicit val jpInt: Parser[Int] = PrimitiveParsers.jpInt;

	/** Interpretador de Long en Json */
	implicit val jpLong: Parser[Long] = PrimitiveParsers.jpLong;

	implicit val jpBigDecimal: Parser[BigDecimal] = PrimitiveParsers.jpBigDecimal;

	implicit val jpDouble: Parser[Double] = PrimitiveParsers.jpDouble;

	implicit val jpFloat: Parser[Float] = PrimitiveParsers.jpFloat;

	////////////

	implicit def jpProduct[P <: Product](implicit helper: ProductParserHelper[P]): Parser[P] = new ProductParser[P](helper)

	implicit def jpCoproduct[C <: Coproduct](implicit helper: CoproductParserHelper[C]): Parser[C] = new CoproductParser[C](helper)

}
