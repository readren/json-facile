package read

import scala.collection.immutable.LinearSeq
import scala.collection.{IterableFactory, mutable}

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}

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

/** A non variant holder of an [[IterableFactory]][IC] instance. Used to suppress the covariant behaviour of the [[IterableFactory]] trait.
 *  @tparam IC the type constructor of the iterable collection for which the wrapped factory generates builders. */
class NonVariantHolderOfAnIterableFactory[IC[_]](val factory: IterableFactory[IC])

object NonVariantHolderOfAnIterableFactory {

	implicit val iterableFactory: NonVariantHolderOfAnIterableFactory[Iterable] = new NonVariantHolderOfAnIterableFactory(Iterable)
	implicit val seqFactory: NonVariantHolderOfAnIterableFactory[Seq] = new NonVariantHolderOfAnIterableFactory(Seq)
	implicit val indexedSeqFactory: NonVariantHolderOfAnIterableFactory[IndexedSeq] = new NonVariantHolderOfAnIterableFactory(IndexedSeq)
	implicit val listFactory: NonVariantHolderOfAnIterableFactory[List] = new NonVariantHolderOfAnIterableFactory(List)
	implicit val vectorFactory: NonVariantHolderOfAnIterableFactory[Vector] = new NonVariantHolderOfAnIterableFactory(Vector)
	implicit val setFactory: NonVariantHolderOfAnIterableFactory[Set] = new NonVariantHolderOfAnIterableFactory(Set)
	implicit val linearSeqFactory: NonVariantHolderOfAnIterableFactory[LinearSeq] = new NonVariantHolderOfAnIterableFactory(LinearSeq)

	implicit val arrayBufferFactory: NonVariantHolderOfAnIterableFactory[mutable.ArrayBuffer] = new NonVariantHolderOfAnIterableFactory(mutable.ArrayBuffer)
	implicit val listBufferFactory: NonVariantHolderOfAnIterableFactory[mutable.ListBuffer] = new NonVariantHolderOfAnIterableFactory(mutable.ListBuffer)
	implicit val queueFactory: NonVariantHolderOfAnIterableFactory[mutable.Queue] = new NonVariantHolderOfAnIterableFactory(mutable.Queue)
	implicit val arrayDequeFactory: NonVariantHolderOfAnIterableFactory[mutable.ArrayDeque] = new NonVariantHolderOfAnIterableFactory(mutable.ArrayDeque)
	implicit val stackFactory: NonVariantHolderOfAnIterableFactory[mutable.Stack] = new NonVariantHolderOfAnIterableFactory(mutable.Stack)
}

