package jsfacile.util

import scala.collection.{MapFactory, immutable, mutable}

/** A non variant holder of an [[scala.collection.MapFactory]][UMC] instance. Used to suppress the covariant behaviour of the [[scala.collection.MapFactory]] trait.
 *
 * @tparam UMC the type constructor of the unsorted map collection for which the wrapped factory generates builders. */
class NonVariantHolderOfAMapFactory[UMC[_, _]](val factory: MapFactory[UMC]);
object NonVariantHolderOfAMapFactory {

	//		type NvhMf[UMC[_, _]] = NonVariantHolderOfAMapFactory[UMC]
	//		@inline private def nvhMf[UMC[_, _]](factory: MapFactory[UMC]) = new NonVariantHolderOfAMapFactory(factory)

	implicit val genericMapFactory: NonVariantHolderOfAMapFactory[scala.collection.Map] = new NonVariantHolderOfAMapFactory(scala.collection.Map)

	implicit val immutableMapFactory: NonVariantHolderOfAMapFactory[immutable.Map] = new NonVariantHolderOfAMapFactory(immutable.Map)
	implicit val immutableHashMapFactory: NonVariantHolderOfAMapFactory[immutable.HashMap] = new NonVariantHolderOfAMapFactory(immutable.HashMap)
	implicit val immutableSeqMapFactory: NonVariantHolderOfAMapFactory[immutable.SeqMap] = new NonVariantHolderOfAMapFactory(immutable.SeqMap)
	implicit val immutableListMapFactory: NonVariantHolderOfAMapFactory[immutable.ListMap] = new NonVariantHolderOfAMapFactory(immutable.ListMap)

	implicit val mutableMapFactory: NonVariantHolderOfAMapFactory[mutable.Map] = new NonVariantHolderOfAMapFactory(mutable.Map)
	implicit val mutableHashMapFactory: NonVariantHolderOfAMapFactory[mutable.HashMap] = new NonVariantHolderOfAMapFactory(mutable.HashMap)
}