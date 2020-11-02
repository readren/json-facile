package jsfacile.api

import jsfacile.read.IterableParser.IterableUpperBound
import jsfacile.read.MapParser.{MapUpperBound, SortedMapUpperBound}
import jsfacile.read.{CursorStr, IterableParser, MapParser, Parser, PrimitiveParsers}
import jsfacile.util.{NonVariantHolderOfAMapFactory, NonVariantHolderOfASortedMapFactory, NonVariantHolderOfAnIterableFactory}

/** Contains the implicit parsers that require import tax.
 * The implicit defined in this package object should be imported in order to have more precedence than the [[jsfacile.read.jpProduct]] and [[jsfacile.read.jpCoproduct]], which should NOT be imported. */
package object read {

	/////////////////
	//// Aliases ////

	type Parser[A] = jsfacile.read.Parser[A]
	type Cursor = jsfacile.read.Parser.Cursor
	type CursorStr = jsfacile.read.CursorStr

	/** Summons a [[Parser]] instance of the specified type */
	def parserOf[A](implicit pa: Parser[A]): Parser[A] = pa

	///////////////////////
	//// Enrich string ////

	/** Adds the [[fromJson]] method to String */
	implicit class FromJsonConvertable(val string: String) extends AnyVal {
		def fromJson[T](implicit pt: Parser[T]): Either[AnyRef, T] = {
			val cursor = new CursorStr(string);
			val result = pt.parse(cursor)
			if (cursor.ok) {
				if (cursor.isPointing)
					Left(s"""The json input was not entirely consumed. The remaining fragment is: "${string.substring(cursor.pos)}".""")
				else
					Right(result)
			} else if (cursor.failed) {
				Left(s"""The parsing failed at position ${cursor.pos} with the message: ${cursor.failureCause}""")
			} else {
				Left("The json representation is not compatible with the expected type")
			}
		}
	}

	//////////////////////////////////////////
	//// Json parsers for primitive types ////

	implicit val jpBoolean: Parser[Boolean] = PrimitiveParsers.jpBoolean

	implicit val jpInt: Parser[Int] = PrimitiveParsers.jpInt;

	implicit val jpLong: Parser[Long] = PrimitiveParsers.jpLong;

	implicit val jpDouble: Parser[Double] = PrimitiveParsers.jpDouble;

	implicit val jpFloat: Parser[Float] = PrimitiveParsers.jpFloat;

	//////////////////////////////////////
	//// Json parsers for basic types ////

	implicit val jpCharSequence: Parser[CharSequence] = PrimitiveParsers.jpString.asInstanceOf[Parser[CharSequence]]

	implicit val jpString: Parser[String] = PrimitiveParsers.jpString

	implicit val jpBigInt: Parser[BigInt] = PrimitiveParsers.jpBigInt;

	implicit val jpBigDecimal: Parser[BigDecimal] = PrimitiveParsers.jpBigDecimal;

	implicit def jpEnumeration[E <: scala.Enumeration](implicit typeTag: scala.reflect.runtime.universe.TypeTag[E]): Parser[E#Value] =
		PrimitiveParsers.jpEnumeration[E](typeTag)

	implicit def jpOption[E](implicit pE: Parser[E]): Parser[Option[E]] = PrimitiveParsers.jpOption(pE);
	implicit def jpSome[E](implicit pE: Parser[E]): Parser[Some[E]] = PrimitiveParsers.jpSome(pE)
	implicit val jpNone: Parser[None.type] = PrimitiveParsers.jpNone

	////////////////////////////////////////////////////////////
	//// Json parser for standard collections library types ////

	implicit def jpIterable[IC[e] <: IterableUpperBound[e], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] =
		IterableParser.apply[IC, E](parserE, factoryHolder)


	implicit def jpUnsortedMap[UMC[k, v] <: MapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfAMapFactory[UMC]
	): Parser[UMC[K, V]] =
		MapParser.apply(parserK, parserV, () => factoryHolder.factory.newBuilder)

	implicit def jpSortedMap[SMC[k, v] <: SortedMapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfASortedMapFactory[SMC],
		orderingK: Ordering[K]
	): Parser[SMC[K, V]] =
		MapParser.apply(parserK, parserV, () => factoryHolder.factory.newBuilder(orderingK))

}
