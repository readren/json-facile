package read

import scala.collection.MapFactory
import scala.collection.immutable.ListMap

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}

object MapParser {

	type LowerBound[K, V] = Map[K, V];

	implicit val mapFactory: MapFactory[Map] = Map
	implicit val listMapFactory: MapFactory[ListMap] = ListMap


	implicit def mapParser[MC[K, V] <: LowerBound[K, V], K, V](
		implicit
		parserK: Parser[K],
		parserV: Parser[V],
		factory: MapFactory[MC]
	): Parser[MC[K, V]] = {

		val entryParser: Parser[(K,  V)] = '[' ~> skipSpaces ~> (parserK <~ skipSpaces) ~ (coma ~> skipSpaces ~> parserV <~ skipSpaces) <~ ']' ^^ {x => (x._1, x._2) }

		'[' ~> skipSpaces ~> (entryParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, factory.newBuilder[K, V]) <~ skipSpaces <~ ']'
	}

}
