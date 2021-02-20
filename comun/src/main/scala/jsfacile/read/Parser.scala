package jsfacile.read

import scala.collection.mutable
import scala.language.implicitConversions

object Parser {

	/** The type of the input elements. [[Int]] was chosen because it is the type that the standard java library uses to represent Unicode code points.
	 * It is not abstract to support other uses because abstract type parameters don't allow specialization and would provoque extra boxing. */
	type Elem = Char
	type Pos = Int

	/** The value returned by [[Parser]]s whose return type is [[Elem]] when the value should be ignored by the caller because the parsing missed or failed.
	 * Any value would be fine. This particular one was chosen because it is very distinctive and also an invalid unicode code point. */
	val IGNORED_ELEM: Elem = '\u0000' // Elegí este pero pudo haber sido cualquier char ya que es ignorado. Idealmente debería ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.

	/** Type class used by [[Parser]]s to create a return value when the parsing missed of failed. The created value is irrelevant because the caller should ignore it. Only the type should match to comfort the compiler. */
	trait Ignore[@specialized +T] {
		def ignored: T
	}
	def ignored[T](implicit ignore: Ignore[T]): T = ignore.ignored;

	implicit val ignoredElem: Ignore[Elem] = new Ignore[Elem] {
		override def ignored: Elem = IGNORED_ELEM
	}

	implicit def ignoredRef[T <: AnyRef]: Ignore[T] = IgnoreRef.asInstanceOf[Ignore[T]]
	object IgnoreRef extends Ignore[AnyRef] {
		override def ignored: Null = null
	}


	/** TODO consider removing ths class and use a tuple instead. That would avoid the creation of an object in the cases where the instance is later converted to a tuple. */
	final case class ~[@specialized(Int, Char) +A, @specialized(Int, Char) +B](_1: A, _2: B) {
		override def toString = s"(${_1}~${_2})"
	}

	/** Creates a [[Parser]] that ever hits (clears the missed flag) and gives the received value as result. The exception is when the failed flag is set, in which case missed flag is not irrelevant.
	 */
	def hit[@specialized(Int, Char) A](a: A): Parser[A] = { cursor =>
		cursor.clearMiss();
		a
	}
	/** Creates a [[Parser]] that ever misses. */
	def miss[@specialized(Int, Char) A]: Parser[A] = { cursor =>
		cursor.miss();
		ignored[A]
	}

	/** Creates a [[Parser]] that ever fails. */
	def fail[@specialized(Int, Char) A](cause: AnyRef): Parser[A] = { cursor =>
		cursor.fail(cause);
		ignored[A]
	}

	/** Creates a [[Parser]] that gives the current [[Cursor]]s position. */
	def pos: Parser[Pos] = {_.pos}

	/** Creates a [[Parser]] that hits if the pointed element equals the received value. In that case advances the [[Cursor]] one position. */
	def acceptElem(elem: Elem): Parser[Elem] = { (cursor: Cursor) =>
		if (cursor.have && cursor.pointedElem == elem) {
			cursor.advance()
			elem
		} else {
			cursor.miss()
			IGNORED_ELEM
		}
	}
	/** Creates a [[Parser]] that hits if the pointed element equals the received [[Char]]. In that case advances the [[Cursor]] one position. */
	implicit def acceptChar(char: Char): Parser[Elem] = acceptElem(char)

	def pick: Parser[Elem] = { cursor =>
		if (cursor.have) {
			cursor.pointedElem
		}
		else {
			cursor.miss()
			IGNORED_ELEM
		}
	}

	/** Creates a [[Parser]] that hits if the sequence starting at the pointed element equals the received [[String]] and misses otherwise. In the hit case the [[Cursor]] is advanced the expected [[String]] length positions and returns the same [[String]]. */
	implicit def acceptStr(seq: String): Parser[String] = { (cursor: Cursor) =>
		if (cursor.comes(seq)) {
			seq
		} else {
			cursor.miss();
			ignored[String]
		}
	}

	/** Creates a [[Parser]] that hits if the sequence starting at the pointed element equals the received [[String]] and misses otherwise. In the hit case the [[Cursor]] is advanced the expected [[String]] length positions and returns the received `opaque` argument. */
	def expectStr[A](str: String, opaque: A): Parser[A] = { cursor =>
		if (!cursor.comes(str)) {
			cursor.miss(s"A $str was expected.")
		}
		opaque
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
				IGNORED_ELEM
			}
		} else {
			cursor.miss()
			IGNORED_ELEM
		}
	}

	def collect[@specialized A](pf: PartialFunction[Elem, A]): Parser[A] = { (cursor: Cursor) =>
		if (cursor.have) {
			pf.applyOrElse(cursor.pointedElem, (_: Elem) => {cursor.miss(); ignored[A];})
		} else {
			ignored[A]
		}
	}

	/** Type class summoner.
	 *
	 * This is the only method in the whole library that uses by name implicit parameter of type [[Parser]][T]. All [[Parser]] summoning for a type `T` that is potentially recursive should use it.*/
	def apply[T](implicit parserT: => Parser[T]): Parser[T] = parserT;
}

/** A parser combinator that minimizes the creation of new object in order to improve speed efficiency, at the cost code legibility.
 * Both the methods of this trait and the [[Parser]]s given by them are thread safe (or should be).
 * 
 * Note that the type parameter is non variant because, if it were, the covariance would cause trouble when using this trait as a type class contract, because Parser[B] is not necessarily a replacement of Parser[A] even if `B <: A`. For example, if A where `Iterable[(String, Int)]` and B where `Map[String, Int]`, the json parser for maps `Parser[Map[String, Int]]` won't be a good replacement of the json parser for iterables `Parser[Iterable[String, Int]]` because the first produces a HashMap and the second a List of tuples, and despite the first have all the functionality of the second, the performance of some operations is worst. Also, the covariance in type clases causes ambiguity problems with non linear hierarchies.
 * A better solution would be to make this trait covariant and wrap it with a non variant wrapper when used as type class contract, but that would be more boilerplate. And the cost of making this trait non variant is low (I believe).
 * */
trait Parser[@specialized(Int, Char) A] { self =>
	import Parser._

	/** The implementation should never call another [[Parser]] instance passing the cursor in failed or missed state. And therefore, can asume that the received cursor is {{{cursor.ok == true}}}. */
	def parse(cursor: Cursor): A

	def map[@specialized(Int, Char) B](f: A => B): Parser[B] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (cursor.ok)
			f(a)
		else
			ignored[B]
	}
	@inline def ^^[@specialized(Int, Char) B](f: A => B): Parser[B] = map(f);
	@inline def ^^^[@specialized(Int, Char) B](b: B): Parser[B] = map(_ => b)

	def flatMap[@specialized(Int, Char) B](f: A => Parser[B]): Parser[B] = { cursor =>
		val a = self.parse(cursor)
		if (cursor.ok)
			f(a).parse(cursor)
		else
			ignored[B]
	}
	def >>[@specialized(Int, Char) B](f: A => Parser[B]): Parser[B] = flatMap(f);

	def pursue[@specialized(Int, Char) B, @specialized(Int, Char) C](iB: Parser[B])(f: (A, B) => C): Parser[C] = { cursor =>
		cursor.attempt { cursor =>
			val a = this.parse(cursor)
			if (cursor.ok) {
				val b = iB.parse(cursor)
				if (cursor.ok) {
					f(a, b)
				} else {
					ignored[C]
				}
			} else {
				ignored[C]
			}
		}
	}
	@inline def ~[@specialized(Int, Char) B](iB: Parser[B]): Parser[~[A, B]] = pursue(iB)(new ~(_, _))

	@inline def ~>[@specialized(Int, Char) B](iB: Parser[B]): Parser[B] = { cursor =>
		cursor.attempt { cursor =>
			this.parse(cursor)
			if (cursor.ok) {
				iB.parse(cursor)
			} else {
				ignored[B]
			}
		}
	}
	@inline def <~[@specialized(Int, Char) B](iB: Parser[B]): Parser[A] = { cursor =>
		cursor.attempt { cursor =>
			val a = this.parse(cursor)
			if (cursor.ok) {
				iB.parse(cursor)
			}
			a
		}
	}


	def orElse(iB: Parser[A]): Parser[A] = { (cursor: Cursor) =>
		val a = cursor.attempt(self.parse);
		if (cursor.missed) {
			cursor.clearMiss();
			iB.parse(cursor)
		} else {
			a
		}
	}
	@inline def |(iB: Parser[A]): Parser[A] = orElse(iB)

	def opt: Parser[Option[A]] =
		self.map(Some(_): Option[A]) | hit(None: Option[A])

	private def repGenFunc[C](builder: mutable.Builder[A, C])(cursor: Cursor): C = {
		var a = self.parse(cursor);
		while (cursor.ok) {
			builder.addOne(a);
			a = self.parse(cursor)
		}
		if (cursor.failed) {
			ignored[C]
		} else {
			cursor.clearMiss();
			builder.result()
		}
	}
	def repGen[C](builderCtor: () => mutable.Builder[A, C]): Parser[C] = { (cursor: Cursor) =>
		this.repGenFunc(builderCtor())(cursor)
	}
	@inline def rep: Parser[List[A]] = repGen(() => List.newBuilder)

	def rep1Gen[C](builderCtor: () => mutable.Builder[A, C]): Parser[C] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (cursor.ok) {
			val builder = builderCtor();
			builder.addOne(a);
			repGenFunc(builder)(cursor)
		} else {
			ignored[C]
		}
	}
	def rep1: Parser[List[A]] = rep1Gen(() => List.newBuilder)

	def repSepGen[@specialized(Int, Char) B, C](iB: Parser[B], builderCtor: () => mutable.Builder[A, C]): Parser[C] = {
		val iB_self = iB ~> self;
		{ cursor =>
			val a = self.parse(cursor);
			if (cursor.failed) {
				ignored[C]
			} else {
				val builder = builderCtor();
				if (cursor.ok) {
					builder.addOne(a);
					iB_self.repGenFunc(builder)(cursor)
				} else {
					cursor.clearMiss();
					builder.result()
				}
			}
		}
	}

	def repSep[@specialized(Int, Char) B](iB: Parser[B]): Parser[List[A]] = repSepGen(iB, () => List.newBuilder)

	def rep1SepGen[@specialized(Int, Char) B, C](iB: Parser[B], builderCtor: () => mutable.Builder[A, C]): Parser[C] = {
		val iB_self = iB ~> self;
		{ cursor =>
			val a = self.parse(cursor);
			if (cursor.ok) {
				val builder = builderCtor();
				builder.addOne(a);
				iB_self.repGenFunc(builder)(cursor);
			} else {
				ignored[C]
			}
		}
	}
	def rep1Sep[@specialized(Int, Char) B](iB: Parser[B]): Parser[List[A]] = rep1SepGen(iB, () => List.newBuilder)

	def repNGen[C](n: Int, builderCtor: () => mutable.Builder[A, C]): Parser[C] = { cursor =>
		if (n > 0) {
			cursor.attempt { cursor =>
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
						ignored[C]
					}
				} else {
					ignored[C]
				}
			}
		} else if (cursor.failed) {
			ignored[C]
		} else {
			builderCtor().result();
		}
	}
	def repN(n: Int): Parser[List[A]] = repNGen(n, () => List.newBuilder)

	/** Set the failure flag if this [[Parser]] misses. */
	def orFail(cause: AnyRef): Parser[A] = { (cursor: Cursor) =>
		val a = self.parse(cursor);
		if (cursor.missed) {
			cursor.fail(cause)
		}
		a
	}

	/** Give a parser that behaves identically to this parser except that, when it misses and no miss cause is specified, sets the received one. */
	def withMissCause(cause: String): Parser[A] = { cursor =>
		val a = this.parse(cursor);
		if (cursor.missed && cursor.missCause == null) {
			cursor.miss(cause)
		}
		a
	}

	/** Gives a parser that behaves like this except when the [[Cursor]] is in failure state. In that case it clears said flag and hits returning the received value.
	 * TODO: take a [[PartialFunction]][AnyRef, B] instead */
	def recover[B >: A](b: B): Parser[B] = { cursor =>
		val a = self.parse(cursor);
		if (cursor.failed) {
			cursor.repair()
			b
		} else {
			a
		}
	}

	/** Gives a parser that behaves like this except when the [[Cursor]] is in failure state. In that case it clears said flag and later behaves like the received parser.
	 * TODO: take a [[PartialFunction]][[[AnyRef]], [[Parser]][B] instead. */
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



