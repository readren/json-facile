package jsfacile

package object joint {

	type IterableUpperBound[E] = scala.collection.Iterable[E];
	type MapUpperBound[K, V] = scala.collection.Map[K, V];
	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V];


	trait Named {
		def name: String;
	}

	@inline def namedOrdering[T <: Named]: Ordering[T] = Ordering.by[T, String](_.name)

}
