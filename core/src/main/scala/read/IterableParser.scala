package read

import scala.collection.IterableFactory
import scala.collection.mutable

import read.Parser.Ignore
import Parser._
import SyntaxParsers.{coma, skipSpaces}
import read.CoproductParserHelper.Coproduct

object IterableParser {

	type LowerBound[E] = Iterable[E];

	implicit val listFactory: IterableFactory[List] = List
	implicit val setFactory: IterableFactory[Set] = Set
	implicit val seqFactory: IterableFactory[Seq] = Seq

//	implicit def iterableBuilder[E, CC[E] <: LowerBound[E]](implicit factory: IterableFactory[CC]): mutable.Builder[E, CC[E]] = factory.newBuilder[E]

//	implicit def jpIterable[C <: LowerBound[E], E](
//		implicit
//		parserE: Parser[E],
//		builder: mutable.Builder[E, C],
//		ignoreE: Ignore[E],
//		ignoreC: Ignore[C]
//	): Parser[C] =
//		'[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, builder) <~ skipSpaces <~ ']'

	implicit def iterableParser[IC[E] <: LowerBound[E], E](
		implicit
		parserE: Parser[E],
		factory: IterableFactory[IC]
	): Parser[IC[E]] =
		'[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, factory.newBuilder[E]) <~ skipSpaces <~ ']'


//	 def jpList[E](implicit parserE: Parser[E], ignoreE: Ignore[E]): Parser[List[E]] = iterableParser[List, E]

}
