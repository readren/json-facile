package jsfacile.util

import scala.collection.immutable.LinearSeq
import scala.collection.{IterableFactory, mutable}

/** A non variant holder of an [[IterableFactory]][IC] instance. Used to suppress the covariant behaviour of the [[IterableFactory]] trait.
 *
 *  @tparam IC the type constructor of the iterable collection for which the wrapped factory generates builders. */
class NonVariantHolderOfAnIterableFactory[IC[_]](val factory: IterableFactory[IC]) {
	val id: String = factory.getClass.getSimpleName
}

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