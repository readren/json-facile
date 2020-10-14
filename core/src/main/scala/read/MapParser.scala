package read

import scala.collection.{MapFactory, SortedMapFactory, mutable}
import scala.collection.immutable.{HashMap, ListMap, SeqMap, SortedMap, TreeMap}

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}

object MapParser {

	//// Unsorted map BEGIN
	type LowerBound[K, V] = scala.collection.Map[K, V];

	implicit val mapFactory: MapFactory[Map] = Map
	implicit val hashMapFactory: MapFactory[HashMap] = HashMap
	implicit val seqMapFactory: MapFactory[SeqMap] = SeqMap
	implicit val listMapFactory: MapFactory[ListMap] = ListMap

	implicit val mutableMapFactory: MapFactory[mutable.Map] = mutable.Map
	implicit val mutableHashMapFactory: MapFactory[mutable.HashMap] = mutable.HashMap

	implicit def unsortedMapParser[UMC[K, V] <: LowerBound[K, V], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factory: MapFactory[UMC]
	): Parser[UMC[K, V]] = {
		mapParser(parserK, parserV, factory.newBuilder)
	}

	//// Unsorted map END
	//// sorted map BEGIN

	type SortedLowerBound[K, V] = scala.collection.SortedMap[K, V]

	implicit val sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap
	implicit val treeMapFactory: SortedMapFactory[TreeMap] = TreeMap

	implicit val mutableSortedMapFactory: SortedMapFactory[mutable.SortedMap] = mutable.SortedMap
	implicit val mutableTreeMapFactory: SortedMapFactory[mutable.TreeMap] = mutable.TreeMap

	implicit def sortedMapParser[SMC[K, V] <: SortedLowerBound[K, V], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factory: SortedMapFactory[SMC],
		orderingK: Ordering[K]
	): Parser[SMC[K, V]] = {
		mapParser(parserK, parserV, factory.newBuilder)
	}

	//// sorted map END
	//// Any map BEGIN

	def mapParser[M <: LowerBound[K, V], K, V](parserK: Parser[K], parserV: Parser[V], builder: mutable.Builder[(K, V), M]): Parser[M] = {

		val entryParser = '[' ~> skipSpaces ~> (parserK <~ skipSpaces) ~ (coma ~> skipSpaces ~> parserV <~ skipSpaces) <~ ']' ^^ {x => (x._1, x._2) }

		'[' ~> skipSpaces ~> (entryParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, builder) <~ skipSpaces <~ ']'
	}
}
