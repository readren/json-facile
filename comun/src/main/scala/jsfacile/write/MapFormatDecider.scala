package jsfacile.write

/** Put an instance of this trait into implicit scope to determine the JSON format of scala map collections: JSON object o JSON array.
 * It was decided to make [[MapFormatDecider]] be contravariant on `V` and `MC` type parameters to allow providing an implicit value with a `val` instead of a `def` when the map value's type or collection type constructor are irrelevant, which uses to be.  For example: {{{implicit val mfd: MapFormatDecider[Foo, Any, SortedMap] = ??? }}} would determine the map format for all maps that extend {{{immutable.SortedMap[Foo, Any]}}}.
 *
 * The default [[MapFormatDecider]] is defined in the macro subproject because it uses the scala reflect API to avoid the necessity of the users of this library to depend on the scala-reflect API at runtime.
 *
 * @tparam K  the type of the map keys.
 * @tparam V  the type of the map values.
 * @tparam MC the map's type constructor. */
trait MapFormatDecider[K, -V, -MC[_, _]] {
	/** Advice: When possible, implement this method with a `val` to improve speed efficiency.
	 *
	 * @return the result determines the JSON format of the map: true -> object, false -> array. */
	def useObject: Boolean;
}

