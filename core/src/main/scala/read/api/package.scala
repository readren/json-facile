package read

import read.IterableParser.IterableUpperBound
import read.MapParser.{MapUpperBound, SortedMapUpperBound}
import util.{NonVariantHolderOfAMapFactory, NonVariantHolderOfASortedMapFactory, NonVariantHolderOfAnIterableFactory}


/** Contains the implicit parsers that require import tax.
 * The implicit defined in this package object should be imported in order to have more precedence than the [[read.jpProduct]] and [[read.jpCoproduct]], which should NOT be imported. */
package object api {
	////////////////
	//// Suggar ////

	/** Adds the [[toJson]] method to all objects */
	implicit class FromJsonConvertable(val string: String) extends AnyVal {
		def fromJson[T](implicit pt: Parser[T]): T =
			pt.parse(new CursorStr(string))
	}

	//////////////////////////////////////////
	//// Supported standard library types ////

	implicit def jpEnumeration[E <: scala.Enumeration](implicit typeTag: scala.reflect.runtime.universe.TypeTag[E]): Parser[E#Value] =
		PrimitiveParsers.jpEnumeration[E](typeTag)


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
