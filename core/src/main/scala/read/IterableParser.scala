package read

import scala.collection.mutable

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}
import util.NonVariantHolderOfAnIterableFactory

object IterableParser {

	type IterableUpperBound[E] = Iterable[E];

	private val cache: mutable.WeakHashMap[(Parser[_], String), Parser[_]] = mutable.WeakHashMap.empty;

	/** @tparam IC iterator type constructor
	 * @tparam E element's type */
	def apply[IC[e] <: IterableUpperBound[e], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] = {
		cache.getOrElseUpdate((parserE, factoryHolder.id), {
			('[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, () => factoryHolder.factory.newBuilder[E]) <~ skipSpaces <~ ']')
				.orFail(s"Invalid syntax for an iterator. The builder factory is ${factoryHolder.factory.getClass.getName}")
		}).asInstanceOf[Parser[IC[E]]]
	}
}



