import read.CoproductParserHelper.Coproduct
import read.IterableParser.IterableUpperBound
import read.MapParser.{MapUpperBound, SortedMapUpperBound}
import util.{NonVariantHolderOfAMapFactory, NonVariantHolderOfASortedMapFactory, NonVariantHolderOfAnIterableFactory}

package object read {

	implicit val jpString: Parser[String] = PrimitiveParsers.jpString

	/** Interpretador de Int en Json */
	implicit val jpInt: Parser[Int] = PrimitiveParsers.jpInt;

	/** Interpretador de Long en Json */
	implicit val jpLong: Parser[Long] = PrimitiveParsers.jpLong;

	implicit val jpBigDecimal: Parser[BigDecimal] = PrimitiveParsers.jpBigDecimal;

	implicit val jpDouble: Parser[Double] = PrimitiveParsers.jpDouble;

	implicit val jpFloat: Parser[Float] = PrimitiveParsers.jpFloat;

	import scala.reflect.runtime.{universe => ru}
	implicit def jpEnumeration[E <: scala.Enumeration](implicit typeTag: ru.TypeTag[E]): Parser[E#Value] = PrimitiveParsers.jpEnumeration[E](typeTag)

	/////////////

	implicit def jpIterable[IC[e] <: IterableUpperBound[e], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] = IterableParser.apply[IC, E](parserE, factoryHolder)


	implicit def jpUnsortedMap[UMC[k, v] <: MapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfAMapFactory[UMC]
	): Parser[UMC[K, V]] = {
		MapParser.apply(parserK, parserV, () => factoryHolder.factory.newBuilder)
	}

	implicit def jpSortedMap[SMC[k, v] <: SortedMapUpperBound[k, v], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfASortedMapFactory[SMC],
		orderingK: Ordering[K]
	): Parser[SMC[K, V]] = {
		MapParser.apply(parserK, parserV, () => factoryHolder.factory.newBuilder(orderingK))
	}

	////////////


	implicit def jpProduct[P <: Product](implicit helper: ProductParserHelper[P]): Parser[P] = new ProductParser[P](helper)

	implicit def jpCoproduct[C <: Coproduct](implicit helper: CoproductParserHelper[C]): Parser[C] = new CoproductParser[C](helper)


	////////////////
	//// Suggar ////

	/** Adds the [[toJson]] method to all objects */
	implicit class FromJsonConvertable(val string: String) extends AnyVal {
		def fromJson[T](implicit pt: Parser[T]): T =
			pt.parse(new CursorStr(string))
	}

}
