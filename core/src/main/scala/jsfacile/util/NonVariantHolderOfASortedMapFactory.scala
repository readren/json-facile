package jsfacile.util

import scala.collection.{SortedMapFactory, immutable, mutable}

/** A non variant holder of an [[SortedMapFactory]][SMC] instance. Used to suppress the covariant behaviour of the [[SortedMapFactory]] trait.
 *
 * @tparam SMC the type constructor of the sorted map collection for which the wrapped factory generates builders. */
class NonVariantHolderOfASortedMapFactory[SMC[_, _]](val factory: SortedMapFactory[SMC]);
object NonVariantHolderOfASortedMapFactory {

	implicit val genSortedMapFactory: NonVariantHolderOfASortedMapFactory[scala.collection.SortedMap] = new NonVariantHolderOfASortedMapFactory(scala.collection.SortedMap)

	implicit val immutableSortedMapFactory: NonVariantHolderOfASortedMapFactory[immutable.SortedMap] = new NonVariantHolderOfASortedMapFactory(immutable.SortedMap)
	implicit val immutableTreeMapFactory: NonVariantHolderOfASortedMapFactory[immutable.TreeMap] = new NonVariantHolderOfASortedMapFactory(immutable.TreeMap)

	implicit val mutableSortedMapFactory: NonVariantHolderOfASortedMapFactory[mutable.SortedMap] = new NonVariantHolderOfASortedMapFactory(mutable.SortedMap)
	implicit val mutableTreeMapFactory: NonVariantHolderOfASortedMapFactory[mutable.TreeMap] = new NonVariantHolderOfASortedMapFactory(mutable.TreeMap)
}
