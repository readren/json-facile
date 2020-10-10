package read

import scala.collection.mutable

object Parser {

	type Elem = Int
	type Pos = Int

	val ELEM_IGNORADO: Elem = 0x8000_0000 // Elegí este pero pudo haber sido cualquier Int negativo. Debe ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.

	/** Type class que da una instancia cualquiera del tipo T. Usado para dar como resultado cuando la interpretación fracasa. */
	trait Ignore[@specialized T] {
		def ignored: T
	}
	def ignored[T](implicit ignora: Ignore[T]): T = ignora.ignored;

	implicit val ignoredCodePoint: Ignore[Elem] = new Ignore[Elem] {
		override def ignored: Elem = ELEM_IGNORADO // Elegí este pero pudo haber sido cualquier Int negativo. Debe ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.
	}

	implicit def ignoredRef[T <: AnyRef]: Ignore[T] = IgnoreRef.asInstanceOf[Ignore[T]]
	object IgnoreRef extends Ignore[AnyRef] {
		override def ignored: AnyRef = null.asInstanceOf[AnyRef]
	}


	/** The parsers receive a cursor which they mutate to communicate the parsing progress to the next parser. This trait models the requirements that that cursor should obey.  */
	trait Cursor {
		/** Current cursor position */
		def pos: Pos;
		/** This cursor missed and failed flags are not set. */
		def ok: Boolean;
		/** This [[Cursor]] is pointing to a valid position. */
		def have: Boolean
		/** Is true when this [[Cursor]] is at the end of the content (the position after the last element). */
		def atEnd: Boolean;
		/** Is true when this [[Cursor]] missed flag is set. */
		def missed: Boolean;
		/** The element being pointed by this [[Cursor]]. */
		def pointedElem: Elem
		def comes(esperado: String): Boolean
		/** Increments the position. */
		def advance(cantPasos: Int = 1): Unit
		/** Sets the missed flag. Parsers set this flag when they miss. The [[Parser.orElse]] operator clears this flag before applying the second parser, when the first has missed. */
		def miss(): Unit
		/** Sets the failed flag. Failures propagates trough all normal parsers until a [[Parser.recover]] or [[Parser.recoverWith]] is reached. */
		def fail(): Unit
		/** Is true when this [[Cursor]] failed flag is set */
		def failed: Boolean;
		/** Removes both, the missed and failed flags. */
		def repair(): Unit

		/** The implementation should execute the received block and, if after that this [[Cursor]] is missed but not failed, should recover the position it had before the block had benn executed.
		 *
		 * @param block procedure which may and usually do modify this cursor.
		 * @return the result given by the block */
		def attempt[@specialized X](block: () => X): X
	}

	final case class ~[@specialized +A, @specialized +B](_1: A, _2: B) {
		override def toString = s"(${_1}~${_2})"
	}

	/** Creates a [[Parser]] that ever hits (clears the missed flag) and gives the received value as result. The exception is when the failed flag is set, in which case missed flag is not cleared.
	 */
	def hit[A](a: A): Parser[A] = { cursor =>
		if (!cursor.failed) cursor.repair();
		a
	}
	/** Creates a [[Parser]] that ever misses. */
	def miss[A](implicit na: Ignore[A]): Parser[A] = { cursor =>
		cursor.miss();
		na.ignored
	}

	/** Creates a [[Parser]] that gives the current [[Cursor]]s position. */
	def pos: Parser[Pos] = {_.pos}

	/** Creates a [[Parser]] that hits if the pointed element equals the received value. In that case advances the [[Cursor]] one position. */
	implicit def acceptElem(elem: Elem): Parser[Elem] = { (cursor: Cursor) =>
		if (cursor.have && cursor.pointedElem == elem) {
			cursor.advance()
			elem
		} else {
			cursor.miss()
			ELEM_IGNORADO
		}
	}
	/** Creates a [[Parser]] that hits if the pointed element equals the received [[Char]]. In that case advances the [[Cursor]] one position. */
	implicit def acceptChar(char: Char): Parser[Elem] = acceptElem(char.toInt)

	/** Creates a [[Parser]] that hits if the sequence starting at the pointed element equals the received [[String]]. In that case advances the [[Cursor]] that [[String]] length positions. */
	implicit def acceptStr(seq: String): Parser[String] = { (cursor: Cursor) =>
		if (cursor.comes(seq)) {
			cursor.advance(seq.length)
			seq
		} else {
			cursor.miss();
			ignored[String]
		}
	}

	/** Creates a [[Parser]] that hits if the received predicate applied to the pointed element is true. In that case advances the [[Cursor]] one position. */
	def acceptElemIf(predicate: Elem => Boolean): Parser[Elem] = { (cursor: Cursor) =>
		if (cursor.have) {
			val ea = cursor.pointedElem;
			if (predicate(ea)) {
				cursor.advance()
				ea
			} else {
				cursor.miss();
				ELEM_IGNORADO
			}
		} else {
			cursor.miss()
			ELEM_IGNORADO
		}
	}

	def collect[A](pf: PartialFunction[Elem, A])(implicit na: Ignore[A]): Parser[A] = { (cursor: Cursor) =>
		if (cursor.have) {
			pf.applyOrElse(cursor.pointedElem, (_: Elem) => {cursor.miss(); na.ignored;})
		} else {
			na.ignored
		}
	}

	/** Type class summoner */
	def apply[T](implicit parserT: Parser[T]): Parser[T] = parserT;
}

/** A parser combinator that minimizes the creation of new object in order to improve speed efficiency, at the cost information about the cause of frustration or failure. Only the position of the incident is reported.
 * Both the methods of this trait and the [[Parser]]s given by them are thread safe (or should be). */
trait Parser[@specialized(Int) A] { self =>
	import Parser._

	def parse(cursor: Cursor): A

	def map[B](f: A => B)(implicit nb: Ignore[B]): Parser[B] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (cursor.ok)
			f(a)
		else
			nb.ignored
	}
	def ^^[B](f: A => B)(implicit nb: Ignore[B]): Parser[B] = map(f);
	def ^^^[B](b: B)(implicit nb: Ignore[B]): Parser[B] = map(_ => b)

	def flatMap[B](f: A => Parser[B])(implicit nb: Ignore[B]): Parser[B] = { cursor =>
		val a = self.parse(cursor)
		if (cursor.ok)
			f(a).parse(cursor)
		else
			nb.ignored
	}
	def >>[B](f: A => Parser[B])(implicit nb: Ignore[B]): Parser[B] = flatMap(f);

	def pursue[B, C](iB: Parser[B])(f: (A, B) => C)(implicit nc: Ignore[C]): Parser[C] = { cursor =>
		cursor.attempt { () =>
			val a = this.parse(cursor)
			if (cursor.ok) {
				val b = iB.parse(cursor)
				if (cursor.ok) {
					f(a, b)
				} else {
					nc.ignored
				}
			} else {
				nc.ignored
			}
		}
	}
	@inline def ~[B](iB: Parser[B]): Parser[~[A, B]] = pursue(iB)(new ~(_, _))
	@inline def ~>[B](ib: Parser[B])(implicit nb: Ignore[B]): Parser[B] = pursue(ib)((_, b) => b)
	@inline def <~[B](ib: Parser[B])(implicit na: Ignore[A]): Parser[A] = pursue(ib)((a, _) => a)


	def orElse[B >: A](iB: Parser[B]): Parser[B] = { (cursor: Cursor) =>
		val a = cursor.attempt { () => self.parse(cursor) };
		if (cursor.ok || cursor.failed) {
			a
		} else {
			cursor.repair();
			iB.parse(cursor)
		}
	}
	@inline def |[B >: A](iB: Parser[B]): Parser[B] = orElse(iB)

	def opt: Parser[Option[A]] = self.map(Some(_)) | hit(None)

	private def repGenFunc[C](builder: mutable.Builder[A, C])(cursor: Cursor)(implicit nc: Ignore[C]): C = {
		var a = self.parse(cursor);
		while (cursor.ok) {
			builder.addOne(a);
			a = self.parse(cursor)
		}
		if (cursor.failed) {
			nc.ignored
		} else {
			cursor.repair();
			builder.result()
		}
	}
	def repGen[C](builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignore[C]): Parser[C] = { (cursor: Cursor) =>
		this.repGenFunc(builderCtor())(cursor)
	}
	@inline def rep: Parser[List[A]] = repGen(() => List.newBuilder)

	def rep1Gen[C](builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignore[C]): Parser[C] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (cursor.ok) {
			val builder = builderCtor();
			builder.addOne(a);
			repGenFunc(builder)(cursor)
		} else {
			nc.ignored
		}
	}
	def rep1: Parser[List[A]] = rep1Gen(() => List.newBuilder)

	def repSepGen[B, C](iB: Parser[B], builder: mutable.Builder[A, C])(implicit na: Ignore[A], nc: Ignore[C]): Parser[C] = {
		val iB_self = iB ~> self;
		val iC: Parser[C] = { cursor =>
			val a = self.parse(cursor);
			if (cursor.ok) {
				builder.addOne(a);
				iB_self.repGenFunc(builder)(cursor)
			} else if (cursor.failed) {
				nc.ignored;
			} else {
				cursor.repair();
				builder.result();
			}
		}
		iC
	}

	def repSep[B](iB: Parser[B])(implicit na: Ignore[A]): Parser[List[A]] = repSepGen(iB, List.newBuilder)

	def rep1SepGen[B, C](iB: Parser[B], builderCtor: () => mutable.Builder[A, C])(implicit na: Ignore[A], nc: Ignore[C]): Parser[C] = {
		val iB_self = iB ~> self
		val iC: Parser[C] = { cursor =>
			val a = self.parse(cursor);
			if (cursor.ok) {
				val builder = builderCtor();
				builder.addOne(a);
				iB_self.repGenFunc(builder)(cursor);
			} else {
				nc.ignored
			}
		}
		iC
	}
	def rep1Sep[B](iB: Parser[B])(implicit na: Ignore[A]): Parser[List[A]] = rep1SepGen(iB, () => List.newBuilder)

	def repNGen[C](n: Int, builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignore[C]): Parser[C] = { cursor =>
		if (n > 0) {
			cursor.attempt { () =>
				var a = self.parse(cursor);
				if (cursor.ok) {
					val builder = builderCtor();
					var cont = n;
					do {
						builder.addOne(a);
						cont -= 1;
						if (cont > 0) {
							a = self.parse(cursor)
						}
					} while (cont > 0 && cursor.ok)
					if (cursor.ok) {
						builder.result()
					} else {
						nc.ignored
					}
				} else {
					nc.ignored
				}
			}
		} else if (cursor.failed) {
			nc.ignored
		} else {
			builderCtor().result();
		}
	}
	def repN(n: Int): Parser[List[A]] = repNGen(n, () => List.newBuilder)

	/** Lanza falla si este [[Parser]] fracasa. */
	def orFail: Parser[A] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (!cursor.ok) {
			cursor.fail()
		}
		a
	}

	/** Gives a parser that behaves like this except when the [[Cursor]] is in failure state. In that case it clears said flag and hits returning the received value. */
	def recover[B >: A](b: B): Parser[B] = { cursor =>
		val a = self.parse(cursor);
		if (cursor.failed) {
			cursor.repair()
			b
		} else {
			a
		}
	}

	/** Gives a parser that behaves like this except when the [[Cursor]] is in failure state. In that case it clears said flag and later behaves like the received parser. */
	def recoverWith[B >: A](iB: Parser[B]): Parser[B] = { cursor =>
		val a = self.parse(cursor);
		if (cursor.failed) {
			cursor.repair();
			iB.parse(cursor)
		} else {
			a
		}
	}
}



