package util

import scala.collection.immutable.{SortedMap, TreeMap}
import scala.collection.{SortedMapFactory, mutable}

/** A non variant holder of an [[SortedMapFactory]][SMC] instance. Used to suppress the covariant behaviour of the [[SortedMapFactory]] trait.
 *
 * @tparam SMC the type constructor of the sorted map collection for which the wrapped factory generates builders. */
class NonVariantHolderOfASortedMapFactory[SMC[_, _]](val factory: SortedMapFactory[SMC]);
object NonVariantHolderOfASortedMapFactory {

	implicit val sortedMapFactory: NonVariantHolderOfASortedMapFactory[SortedMap] = new NonVariantHolderOfASortedMapFactory(SortedMap)
	implicit val treeMapFactory: NonVariantHolderOfASortedMapFactory[TreeMap] = new NonVariantHolderOfASortedMapFactory(TreeMap)

	implicit val mutableSortedMapFactory: NonVariantHolderOfASortedMapFactory[mutable.SortedMap] = new NonVariantHolderOfASortedMapFactory(mutable.SortedMap)
	implicit val mutableTreeMapFactory: NonVariantHolderOfASortedMapFactory[mutable.TreeMap] = new NonVariantHolderOfASortedMapFactory(mutable.TreeMap)
}
