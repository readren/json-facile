package read

import scala.collection.immutable.LinearSeq
import scala.collection.{IterableFactory, mutable}

import read.Parser._
import read.SyntaxParsers.{coma, skipSpaces}

object IterableParser {

	type LowerBound[E] = Iterable[E];

	implicit val iterableFactory: IterableFactory[Iterable] = Iterable
	implicit val seqFactory: IterableFactory[Seq] = Seq
	implicit val indexedSeqFactory: IterableFactory[IndexedSeq] = IndexedSeq
	implicit val listFactory: IterableFactory[List] = List
	implicit val vectorFactory: IterableFactory[Vector] = Vector
	implicit val setFactory: IterableFactory[Set] = Set
	implicit val linearSeqFactory: IterableFactory[LinearSeq] = LinearSeq

	implicit val arrayBufferFactory: IterableFactory[mutable.ArrayBuffer] = mutable.ArrayBuffer
	implicit val listBufferFactory: IterableFactory[mutable.ListBuffer] = mutable.ListBuffer
	implicit val queueFactory: IterableFactory[mutable.Queue] = mutable.Queue
	implicit val arrayDequeFactory: IterableFactory[mutable.ArrayDeque] = mutable.ArrayDeque
	implicit val stackFactory: IterableFactory[mutable.Stack] = mutable.Stack

	implicit def iterableParser[IC[E] <: LowerBound[E], E](
		implicit
		parserE: Parser[E],
		factory: IterableFactory[IC]
	): Parser[IC[E]] =
		'[' ~> skipSpaces ~> (parserE <~ skipSpaces).repSepGen(coma <~ skipSpaces, factory.newBuilder[E]) <~ skipSpaces <~ ']'
}
