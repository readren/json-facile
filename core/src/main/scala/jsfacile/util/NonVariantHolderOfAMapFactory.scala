package jsfacile.util

import scala.collection.immutable.{HashMap, ListMap, SeqMap}
import scala.collection.{MapFactory, mutable}

/** A non variant holder of an [[MapFactory]][UMC] instance. Used to suppress the covariant behaviour of the [[MapFactory]] trait.
 *
 * @tparam UMC the type constructor of the unsorted map collection for which the wrapped factory generates builders. */
class NonVariantHolderOfAMapFactory[UMC[_, _]](val factory: MapFactory[UMC]);
object NonVariantHolderOfAMapFactory {

	//		type NvhMf[UMC[_, _]] = NonVariantHolderOfAMapFactory[UMC]
	//		@inline private def nvhMf[UMC[_, _]](factory: MapFactory[UMC]) = new NonVariantHolderOfAMapFactory(factory)

	implicit val mapFactory: NonVariantHolderOfAMapFactory[Map] = new NonVariantHolderOfAMapFactory(Map)
	implicit val hashMapFactory: NonVariantHolderOfAMapFactory[HashMap] = new NonVariantHolderOfAMapFactory(HashMap)
	implicit val seqMapFactory: NonVariantHolderOfAMapFactory[SeqMap] = new NonVariantHolderOfAMapFactory(SeqMap)
	implicit val listMapFactory: NonVariantHolderOfAMapFactory[ListMap] = new NonVariantHolderOfAMapFactory(ListMap)

	implicit val mutableMapFactory: NonVariantHolderOfAMapFactory[mutable.Map] = new NonVariantHolderOfAMapFactory(mutable.Map)
	implicit val mutableHashMapFactory: NonVariantHolderOfAMapFactory[mutable.HashMap] = new NonVariantHolderOfAMapFactory(mutable.HashMap)
}