package read

import scala.collection.immutable.LinearSeq
import scala.collection.{IterableFactory, mutable}

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}

object IterableParser {

	/** A non variant holder of an [[IterableFactory]][IC] instance. Used to suppress the covariant behaviour of the [[IterableFactory]] trait.
	 *  @tparam IC the type constructor of the iterable collection for which the wrapped factory generates builders. */
	class NonVariantHolderOfAnIterableFactory[IC[_]](val factory: IterableFactory[IC])

	object NonVariantHolderOfAnIterableFactory {

		@inline def nvhIf[IC[_]](f: IterableFactory[IC]) = new NonVariantHolderOfAnIterableFactory[IC](f)
		type NvhIf[IC[_]] = NonVariantHolderOfAnIterableFactory[IC]

		implicit val iterableFactory: NvhIf[Iterable] = nvhIf(Iterable)
		implicit val seqFactory: NvhIf[Seq] = nvhIf(Seq)
		implicit val indexedSeqFactory: NvhIf[IndexedSeq] = nvhIf(IndexedSeq)
		implicit val listFactory: NvhIf[List] = nvhIf(List)
		implicit val vectorFactory: NvhIf[Vector] = nvhIf(Vector)
		implicit val setFactory: NvhIf[Set] = nvhIf(Set)
		implicit val linearSeqFactory: NvhIf[LinearSeq] = nvhIf(LinearSeq)

		implicit val arrayBufferFactory: NvhIf[mutable.ArrayBuffer] = nvhIf(mutable.ArrayBuffer)
		implicit val listBufferFactory: NvhIf[mutable.ListBuffer] = nvhIf(mutable.ListBuffer)
		implicit val queueFactory: NvhIf[mutable.Queue] = nvhIf(mutable.Queue)
		implicit val arrayDequeFactory: NvhIf[mutable.ArrayDeque] = nvhIf(mutable.ArrayDeque)
		implicit val stackFactory: NvhIf[mutable.Stack] = nvhIf(mutable.Stack)
	}

	type LowerBound[E] = Iterable[E];

	/** @tparam IC iterator type constructor
	 * @tparam E element's type */
	implicit def iterableParser[IC[E] <: LowerBound[E], E](
		implicit
		parserE: Parser[E],
		factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
	): Parser[IC[E]] =
		'[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, factoryHolder.factory.newBuilder[E]) <~ skipSpaces <~ ']'
}
