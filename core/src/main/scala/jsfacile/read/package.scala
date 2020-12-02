package jsfacile

import jsfacile.macros.{EnumParserMacro, NothingMacros}


/** It is not necessary to import any implicit defined in this package object. The compiler finds them anyway because the [[Parser]] trait is defined in the same package. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Parser]] accesible without prefix (imported or declared in the block scope). */
package object read extends PriorityMediumParsers {

	//////////////////////////////////////////
	//// Json parsers for primitive types ////

	implicit val jpUnit: Parser[Unit] = BasicParsers.jpUnit

	implicit val jpNull: Parser[Null] = BasicParsers.jpNull

	implicit val jpBoolean: Parser[Boolean] = BasicParsers.jpBoolean

	implicit val jpInt: Parser[Int] = BasicParsers.jpInt;

	implicit val jpLong: Parser[Long] = BasicParsers.jpLong;

	implicit val jpDouble: Parser[Double] = BasicParsers.jpDouble;

	implicit val jpFloat: Parser[Float] = BasicParsers.jpFloat;

	implicit def jpNothing: Parser[Nothing] = macro NothingMacros.materializeNothingParserImpl

	//////////////////////////////////////
	//// Json parsers for basic types ////

	implicit val jpCharSequence: Parser[CharSequence] = BasicParsers.jpString.asInstanceOf[Parser[CharSequence]]

	implicit val jpString: Parser[String] = BasicParsers.jpString

	implicit val jpBigInt: Parser[BigInt] = BasicParsers.jpBigInt;

	implicit val jpBigDecimal: Parser[BigDecimal] = BasicParsers.jpBigDecimal;

	implicit def jpEnumeration[E <: scala.Enumeration]: Parser[E#Value] = macro EnumParserMacro.materializeImpl[E]

	implicit def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = BasicParsers.jpOption(pE);
	implicit def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = BasicParsers.jpSome(pE)
	implicit val jpNone: Parser[None.type] = BasicParsers.jpNone

	implicit def jpEither[L, R](implicit pL: Parser[L], pR: Parser[R]): Parser[Either[L, R]] = BasicParsers.jpEither
	implicit def jpLeft[L, R](implicit pL: Parser[L]): Parser[Left[L, R]] = BasicParsers.jpLeft
	implicit def jpRight[L, R](implicit pR: Parser[R]): Parser[Right[L, R]] = BasicParsers.jpRight

}
