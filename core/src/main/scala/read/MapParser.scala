package read

import scala.collection.mutable

import read.Parser._
import read.SyntaxParsers.{colon, coma, skipSpaces, string}

object MapParser {

	type MapUpperBound[K, V] = scala.collection.Map[K, V];

	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V]

	def apply[M <: MapUpperBound[K, V], K, V](parserK: Parser[K], parserV: Parser[V], builderCtor: () => mutable.Builder[(K, V), M]): Parser[M] = {

		val pairParser = '[' ~> skipSpaces ~> (parserK <~ skipSpaces) ~ (coma ~> skipSpaces ~> parserV <~ skipSpaces) <~ ']' ^^ { x => (x._1, x._2) }
		val asArray = '[' ~> skipSpaces ~> (pairParser <~ skipSpaces).repSepGen(coma <~ skipSpaces, builderCtor) <~ skipSpaces <~ ']'

		val entryParser = ((string <~ skipSpaces <~ colon <~ skipSpaces) ~ (parserV <~ skipSpaces)).flatMap[(K, V)]{ case keyStr ~ value =>
			if (parserK == PrimitiveParsers.jpString) {
				hit(keyStr.asInstanceOf[K] -> value)
			} else {
				val cursor = new CursorStr(keyStr);
				val key = parserK.parse(cursor)
				if (cursor.ok && cursor.atEnd) hit(key -> value)
				else fail(s"The map key parser failed to interpret the key embedded in the field name.")
			}
		}
		val asObject = '{' ~> skipSpaces ~> entryParser.repSepGen[Pos, M](coma <~ skipSpaces, builderCtor) <~ skipSpaces <~ '}'

		(asArray | asObject).orFail(s"Invalid syntax for a map.")
	}
}