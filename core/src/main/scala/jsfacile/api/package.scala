package jsfacile

package object api {
	//////////////////////
	//// Aliases ////

	type IterableUpperBound[E] = scala.collection.Iterable[E];
	type MapUpperBound[K, V] = scala.collection.Map[K, V];
	type SortedMapUpperBound[K, V] = scala.collection.SortedMap[K, V];

	type Parser[A] = jsfacile.read.Parser[A]
	val Parser: jsfacile.read.Parser.type = jsfacile.read.Parser;
	type Cursor = jsfacile.read.Cursor;
	type CursorStr = jsfacile.read.CursorStr;

	type Appender[A] = jsfacile.write.Appender[A];
	val Appender: jsfacile.write.Appender.type = jsfacile.write.Appender;
	type Record = jsfacile.write.Record;
	type RecordStr = jsfacile.write.RecordStr;

	type discriminatorField = jsfacile.annotations.discriminatorField;
	type DiscriminatorDecider[C, F <: AnyAdt] = jsfacile.joint.DiscriminatorDecider[C, F]
	type DiscriminatorConf = jsfacile.joint.DiscriminatorConf;
	type DiscriminatorValueMapper[C, F <: AnyAdt] = jsfacile.joint.DiscriminatorValueMapper[C, F]

	type AnyAdt = jsfacile.joint.AnyAdt;
	type ProductsOnly = jsfacile.joint.ProductsOnly;
	type CoproductsOnly = jsfacile.joint.CoproductsOnly;

	type MapFormatDecider[K, V, MC[_, _]] = jsfacile.write.MapFormatDecider[K, V, MC];
	type PrefixInserter[A, F <: AnyAdt] = jsfacile.write.PrefixInserter[A, F]

	type JsDocument = jsfacile.jsonast.JsDocument;

	///////////////////
	//// Summoners ////

	/** Summons a [[Parser]] instance of the specified type */
	def parserOf[A](implicit pa: Parser[A]): Parser[A] = pa;

	/** Summons an [[Appender]] instance of the specified type */
	def appenderOf[A](implicit aoa: Appender[A]): Appender[A] = aoa;


	///////////////////////////////////
	//// Existent types enrichment ////

	/** Adds the [[toJson]] method to all objects */
	implicit class ToJsonConvertible[T](val obj: T) extends AnyVal {
		/** Translates to JSON [[java.lang.String]] using the [[jsfacile.write.Appender]] of the specified supertype `S`.
		 *
		 * Useful to avoid the automatic derivation of an [[Appender]] for `T`` when one for `S` already exists.
		 *
		 * Also useful when it is necessary that the [[Appender]] includes a discriminator, provided the [[jsfacile.api.DiscriminatorDecider]] for `S` has [[jsfacile.joint.DiscriminatorDecider.required]] `== true` */
		def toJsonAs[S >: T](implicit as: Appender[S]): String = {
			val r = new RecordStr(new java.lang.StringBuilder());
			as.append(r, obj);
			r.sb.toString;
		}

		/** Translate to a JSON [[java.lang.String]]. */
		@inline
		def toJson(implicit at: Appender[T]): String = toJsonAs[T](at)

		/** Translate to a [[JsDocument]] using the [[jsfacile.write.Appender]] of the specified supertype `S`.
		 *
		 * Useful to avoid the automatic derivation of an [[Appender]] for `T`` when one for `S` already exists.
		 *
		 * Also useful when it is necessary that the [[Appender]] includes a discriminator, provided the [[jsfacile.api.DiscriminatorDecider]] for `S` has [[jsfacile.joint.DiscriminatorDecider.required]] `== true` */
		@inline
		def toJsDocumentAs[S >: T](implicit as: Appender[S]): JsDocument = new JsDocument(this.toJsonAs[S](as))

		/** Translate to a [[JsDocument]]. */
		@inline
		def toJsDocument(implicit at: Appender[T]): JsDocument = new JsDocument(this.toJsonAs[T](at))
	}

	/** Adds the [[fromJson]] method to instances of [[String]]. */
	implicit class FromJsonStringConvertible(val stringDoc: String) extends AnyVal {
		/** Tries to create an instance of the specified type with the value represented by this [[java.lang.String]] in JSON format.
		 *
		 * @tparam T the type of the instance to be created. This type parameter should be specified explicitly. */
		def fromJson[T](implicit pt: Parser[T]): Either[ParseError, T] = {
			Parser.parse(stringDoc.toCharArray)(pt)
		}
	}

	/** Adds the [[fromJson]] method to instances of [[Array]][Char].*/
	implicit class FromJsonCharArrayConvertible(val arrayDoc: Array[Char]) extends AnyVal {
		/** Tries to create an instance of the specified type with the value represented by this [[java.lang.String]] in JSON format.
		 *
		 * @tparam T the type of the instance to be created. This type parameter should be specified explicitly. */
		def fromJson[T](implicit pt: Parser[T]): Either[ParseError, T] = {
			Parser.parse(arrayDoc)(pt)
		}
	}
}
