package read

import scala.collection.{MapFactory, SortedMapFactory, mutable}
import scala.collection.immutable.{HashMap, ListMap, SeqMap, SortedMap, TreeMap}

import read.Parser._
import read.SyntaxParsers.{coma, colon, skipSpaces, string}

object MapParser {

	//// Unsorted map BEGIN

	type LowerBound[K, V] = scala.collection.Map[K, V];

	implicit def unsortedMapParser[UMC[K, V] <: LowerBound[K, V], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfAMapFactory[UMC]
	): Parser[UMC[K, V]] = {
		mapParser(parserK, parserV, factoryHolder.factory.newBuilder)
	}
	//// Unsorted map END

	//// sorted map BEGIN

	type SortedLowerBound[K, V] = scala.collection.SortedMap[K, V]

	implicit def sortedMapParser[SMC[K, V] <: SortedLowerBound[K, V], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factoryHolder: NonVariantHolderOfASortedMapFactory[SMC],
		orderingK: Ordering[K]
	): Parser[SMC[K, V]] = {
		mapParser(parserK, parserV, factoryHolder.factory.newBuilder)
	}
	//// sorted map END

	//// Any map BEGIN

	def mapParser[M <: LowerBound[K, V], K, V](parserK: Parser[K], parserV: Parser[V], builder: mutable.Builder[(K, V), M]): Parser[M] = {

		val pairParser = '[' ~> skipSpaces ~> (parserK <~ skipSpaces) ~ (coma ~> skipSpaces ~> parserV <~ skipSpaces) <~ ']' ^^ { x => (x._1, x._2) }
		val asArray = '[' ~> skipSpaces ~> (pairParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, builder) <~ skipSpaces <~ ']'

		val entryParser = ((string <~ skipSpaces <~ colon <~ skipSpaces) ~ (parserV <~ skipSpaces)).flatMap[(K, V)]{ case keyStr ~ value =>
			if (parserK == string) {
				hit(keyStr.asInstanceOf[K] -> value)
			} else {
				val cursor = new CursorStr(keyStr);
				val key = parserK.parse(cursor)
				if (cursor.ok && cursor.atEnd) hit(key -> value)
				else miss
			}
		}
		val asObject = '{' ~> skipSpaces ~> entryParser.repSepGen[Pos, M](coma <~ skipSpaces, builder) <~ skipSpaces <~ '}'

		asArray | asObject
	}
	//// Any map END
}


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
