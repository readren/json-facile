package jsfacile.util

import scala.collection.{IterableFactory, immutable, mutable}

/** A non variant holder of an [[IterableFactory]][IC] instance. Used to suppress the covariant behaviour of the [[IterableFactory]] trait.
 *
 *  @tparam IC the type constructor of the iterable collection for which the wrapped factory generates builders. */
class NonVariantHolderOfAnIterableFactory[IC[_]](val factory: IterableFactory[IC]) {
	val id: String = factory.getClass.getSimpleName
}

object NonVariantHolderOfAnIterableFactory {

	implicit val genIterableFactory: NonVariantHolderOfAnIterableFactory[scala.collection.Iterable] = new NonVariantHolderOfAnIterableFactory(scala.collection.Iterable)
	implicit val genSeqFactory: NonVariantHolderOfAnIterableFactory[scala.collection.Seq] = new NonVariantHolderOfAnIterableFactory(scala.collection.Seq)
	implicit val genIndexedSeqFactory: NonVariantHolderOfAnIterableFactory[scala.collection.IndexedSeq] = new NonVariantHolderOfAnIterableFactory(scala.collection.IndexedSeq)
	implicit val genLinearSeqFactory: NonVariantHolderOfAnIterableFactory[scala.collection.LinearSeq] = new NonVariantHolderOfAnIterableFactory(scala.collection.LinearSeq)
	implicit val genSetFactory: NonVariantHolderOfAnIterableFactory[scala.collection.Set] = new NonVariantHolderOfAnIterableFactory(scala.collection.Set)

	implicit val immutableIterableFactory: NonVariantHolderOfAnIterableFactory[immutable.Iterable] = new NonVariantHolderOfAnIterableFactory(immutable.Iterable)
	implicit val immutableSeqFactory: NonVariantHolderOfAnIterableFactory[immutable.Seq] = new NonVariantHolderOfAnIterableFactory(immutable.Seq)
	implicit val immutableIndexedSeqFactory: NonVariantHolderOfAnIterableFactory[immutable.IndexedSeq] = new NonVariantHolderOfAnIterableFactory(immutable.IndexedSeq)
	implicit val immutableLinearSeqFactory: NonVariantHolderOfAnIterableFactory[immutable.LinearSeq] = new NonVariantHolderOfAnIterableFactory(immutable.LinearSeq)
	implicit val immutableListFactory: NonVariantHolderOfAnIterableFactory[immutable.List] = new NonVariantHolderOfAnIterableFactory(immutable.List)
	implicit val immutableVectorFactory: NonVariantHolderOfAnIterableFactory[immutable.Vector] = new NonVariantHolderOfAnIterableFactory(immutable.Vector)
	implicit val immutableSetFactory: NonVariantHolderOfAnIterableFactory[immutable.Set] = new NonVariantHolderOfAnIterableFactory(immutable.Set)

	implicit val mutableIterableFactory: NonVariantHolderOfAnIterableFactory[mutable.Iterable] = new NonVariantHolderOfAnIterableFactory(mutable.Iterable)
	implicit val mutableArrayBufferFactory: NonVariantHolderOfAnIterableFactory[mutable.ArrayBuffer] = new NonVariantHolderOfAnIterableFactory(mutable.ArrayBuffer)
	implicit val mutableListBufferFactory: NonVariantHolderOfAnIterableFactory[mutable.ListBuffer] = new NonVariantHolderOfAnIterableFactory(mutable.ListBuffer)
	implicit val mutableQueueFactory: NonVariantHolderOfAnIterableFactory[mutable.Queue] = new NonVariantHolderOfAnIterableFactory(mutable.Queue)
	implicit val mutableArrayDequeFactory: NonVariantHolderOfAnIterableFactory[mutable.ArrayDeque] = new NonVariantHolderOfAnIterableFactory(mutable.ArrayDeque)
	implicit val mutableStackFactory: NonVariantHolderOfAnIterableFactory[mutable.Stack] = new NonVariantHolderOfAnIterableFactory(mutable.Stack)
}