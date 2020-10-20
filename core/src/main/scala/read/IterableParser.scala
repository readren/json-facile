package read

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}
import util.NonVariantHolderOfAnIterableFactory

object IterableParser {

	type LowerBound[E] = Iterable[E];

	/** @tparam IC iterator type constructor
	 * @tparam E element's type */
	implicit def iterableParser[IC[X] <: LowerBound[X], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] =
		('[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, () => factoryHolder.factory.newBuilder[E]) <~ skipSpaces <~ ']').orFail(s"Invalid syntax for an iterator. The builder factory is ${factoryHolder.factory.getClass.getName}")
}



