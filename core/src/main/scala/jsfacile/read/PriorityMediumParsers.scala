package jsfacile.read

import scala.reflect.ClassTag

import jsfacile.api.{IterableUpperBound, MapUpperBound, SortedMapUpperBound}
import jsfacile.util.{NonVariantHolderOfAMapFactory, NonVariantHolderOfASortedMapFactory, NonVariantHolderOfAnIterableFactory}

/** It is not necessary to import any implicit defined in this trait. The compiler finds them anyway because the [[jsfacile.read]] package object implements it; and said package is where the [[Parser]] trait is defined. Remember that implicits defined in a package object are part of the implicit scope of a type prefixed by that package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Parser]] accesible without prefix (imported or declared in the enclosing scope). */
trait PriorityMediumParsers extends PriorityLowParsers {

	////////////////////////////////////////////////////////////
	//// Json parser for standard collections library types ////

	implicit def jpArray[E](implicit parserE: Parser[E], ctE: ClassTag[E]): Parser[Array[E]] =
		new ArrayParser[E](parserE, ctE);

	implicit def jpIterable[IC[e] <: IterableUpperBound[e], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] =
		new IterableParser[IC, E](parserE, factoryHolder);

	implicit def jpUnsortedMap[UMC[k, v] <: MapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		parserString: Parser[String],
		factoryHolder: NonVariantHolderOfAMapFactory[UMC]
	): Parser[UMC[K, V]] =
		new MapParser(parserK, parserV, parserString, () => factoryHolder.factory.newBuilder);

	implicit def jpSortedMap[SMC[k, v] <: SortedMapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		parserString: Parser[String],
		factoryHolder: NonVariantHolderOfASortedMapFactory[SMC],
		orderingK: Ordering[K]
	): Parser[SMC[K, V]] =
		new MapParser(parserK, parserV, parserString, () => factoryHolder.factory.newBuilder(orderingK));
}
