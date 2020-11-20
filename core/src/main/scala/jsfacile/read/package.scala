package jsfacile

import jsfacile.api.{IterableUpperBound, MapUpperBound, SortedMapUpperBound}
import jsfacile.macros.{CoproductParserHelper, CoproductUpperBound, EnumParserMacro, ProductParserHelper, ProductUpperBound, SingletonParserHelper}
import jsfacile.util.{NonVariantHolderOfAMapFactory, NonVariantHolderOfASortedMapFactory, NonVariantHolderOfAnIterableFactory}


/** It is not necessary to import any implicit defined in this package object. The compiler finds them anyway because the [[Parser]] trait is defined in the same package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Parser]] accesible without prefix (imported or declared in the block scope). */
package object read {

	//////////////////////////////////////////
	//// Json parsers for primitive types ////

	implicit val jpUnit: Parser[Unit] = BasicParsers.jpUnit

	implicit val jpNull: Parser[Null] = BasicParsers.jpNull

	implicit val jpBoolean: Parser[Boolean] = BasicParsers.jpBoolean

	implicit val jpInt: Parser[Int] = BasicParsers.jpInt;

	implicit val jpLong: Parser[Long] = BasicParsers.jpLong;

	implicit val jpDouble: Parser[Double] = BasicParsers.jpDouble;

	implicit val jpFloat: Parser[Float] = BasicParsers.jpFloat;

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

//	private val jpProductsCache = mutable.WeakHashMap.empty[String, ProductParser[_ <: ProductUpperBound]]

	implicit def jpProduct[P <: ProductUpperBound](implicit helper: ProductParserHelper[P]): ProductParser[P] = {
//		jpProductsCache.getOrElseUpdate(
//			helper.fullName,
			new ProductParser[P](helper)
//		).asInstanceOf[ProductParser[P]]
	}

	///////////////////////////////////////////////////////////////////
	//// Json parser for sealed trait and sealed abstract classes  ////

//	private val jpCoproductsCache = mutable.WeakHashMap.empty[String, CoproductParser[_ <: CoproductUpperBound]]

	implicit def jpCoproduct[C <: CoproductUpperBound](implicit helper: CoproductParserHelper[C]): CoproductParser[C] = {
//		jpCoproductsCache.getOrElseUpdate(
//			helper.fullName,
			new CoproductParser[C](helper)
//		).asInstanceOf[CoproductParser[C]]
	}

}
