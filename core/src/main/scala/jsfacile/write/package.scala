package jsfacile

import scala.collection.mutable

import jsfacile.joint.{CoproductUpperBound, ProductUpperBound}
import jsfacile.macros.{CoproductAppenderHelper, ProductAppender}
import jsfacile.write.MapAppender.{MapFormatDecider, defaultMapFormatDecider}

/** It is not necessary to import any implicit defined in this package object. The compiler finds them anyway because the [[Appender]] trait is defined in the same package.
 *  Also, it is not recommended to import any of them so that they have lower precedence than any [[Appender]] accesible without prefix (imported or declared in the block scope). */
package object write {

	////////////////////////////////////////////
	//// Json appenders for primitive types ////

	implicit val jaNull: Appender[Null] = { (r: Record, _) => r.append("null") }

	implicit val jaBoolean: Appender[Boolean] = { (r, bool) => r.append(if (bool) "true" else "false") }

	implicit val jaInt: Appender[Int] = { (r, int) => r.append(int) }

	implicit val jaLong: Appender[Long] = { (r, long) => r.append(long) }

	implicit val jaFloat: Appender[Float] = { (r, float) =>
		if (float.isNaN) r.append("null")
		else r.append(float)
	}

	implicit val jaDouble: Appender[Double] = { (r, double) =>
		if (double.isNaN) r.append("null")
		else r.append(double)
	}

	implicit val jaChar: Appender[Char] = { (r, char) =>
		r.append('"');
		encodeStringChar(r, char).append('"')
	}

	////////////////////////////////////////
	//// Json appenders for basic types ////

	implicit val jaCharSequence: Appender[CharSequence] = { (r, csq) =>
		r.append('"');
		encodeStringCharSequence(r, csq).append('"')
	}

	implicit val jaString: Appender[String] = jaCharSequence.asInstanceOf[Appender[String]]

	implicit val jaBigInt: Appender[BigInt] = { (r, bigInt) => r.append(bigInt.toString()) }

	implicit val jaBigDecimal: Appender[BigDecimal] = { (r, bigDec) => r.append(bigDec.toString()) }

	// TODO add a implicit parameter which decides how would the enums be identified, by name or by id.
	implicit def jaEnumeration[E <: scala.Enumeration]: Appender[E#Value] =
		(r, enum) => jaCharSequence.append(r, enum.toString)

	implicit def jaOption[A](implicit appenderA: Appender[A]): Appender[Option[A]] = (r, oa) =>
		oa match {
			case Some(a) => appenderA.append(r, a)
			case None => jaNull.append(r, null)
		}
	implicit def jaSome[A](implicit appenderA: Appender[A]): Appender[Some[A]] = (r, sa) => appenderA.append(r, sa.get)
	implicit val jaNone: Appender[None.type] = (r, _) => r.append("null");

	///////////////////////////////////////////////////////////////
	//// Json appenders for standard collections library types ////

	@inline implicit def jaIterable[E, IC[e] <: Iterable[e]](implicit elemAppender: Appender[E]): Appender[IC[E]] =
		IterableAppender.apply[E, IC](elemAppender)

	@inline implicit def jaMap[K, V, MC[k, v] <: Map[k, v]](
		implicit
		ka: Appender[K],
		va: Appender[V],
		mfd: MapFormatDecider[K, V, MC[K, V]] = defaultMapFormatDecider
	): Appender[MC[K, V]] =
		MapAppender.apply[K, V, MC](ka, va, mfd)

	/////////////////////////////////////////////
	//// Json appenders for concrete classes ////

	implicit def jaProduct[P <: ProductUpperBound]: Appender[P] = macro ProductAppender.materializeImpl[P]

	///////////////////////////////////////////////////////////////////////
	//// Json appenders for sealed traits and sealed abstract classes  ////

	private val jaCoproductCache = mutable.WeakHashMap.empty[String, CoproductAppender[_ <: CoproductUpperBound]]

	implicit def jaCoproduct[C <: CoproductUpperBound](implicit helper: CoproductAppenderHelper[C]): CoproductAppender[C] = {
		jaCoproductCache.getOrElseUpdate(helper.fullName, new CoproductAppender(helper)).asInstanceOf[CoproductAppender[C]]
	};

	//////////////////////////////
	//// Json string enconders ////

	/** Encodes the received [[Char]] in order to be part of a json string
	 * Surrogate chars are not altered. */
	def encodeStringChar(r: Record, char: Char): Record = {
		if (char == '"') {
			r.append('\\').append(char)
		} else if (char == '\\') {
			r.append('\\').append('\\')
		} else if (char < 32) {
			r.append('\\');
			char match {
				case '\t' => r.append('t')
				case '\n' => r.append('n')
				case '\r' => r.append('r')
				case '\f' => r.append('f')
				case '\b' => r.append('b')
				case ctrl =>
					r.append("u00")
					if (ctrl < 16)
						r.append('0')
					r.append(Integer.toHexString(ctrl))
			}
			r
		} else {
			r.append(char)
		}
	}

	/** Encodes the received [[CharSequence]] in order to be part of a json string */
	def encodeStringCharSequence(r: Record, csq: CharSequence): Record = {
		var index = 0;
		val length = csq.length;
		while (index < length) {
			// Note that it is not necessary to treat surrogate chars differently because the encodeStringChar method don't alters them.
			encodeStringChar(r, csq.charAt(index));
			index += 1;
		}
		r
	}
}
