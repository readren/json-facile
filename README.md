# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types and String JSON documents directly, without any intermediate representation.
* An efficient JSON parser. Considerably faster (around 33%) than [spray]. If the JSON contains ignored fields, _jsson-facile_ is even faster.
* Type-class based conversion (no runtime reflection, no intrusion).
* Automatic derivation: the conversion type-classes of custom ADTs (abstract data types) are automaticaly generated at compile-time by macros. Zero boilerplate.
* The automatic derivation works for any concrete data type. It's not required to be a case class nor inherit `scala.Product`. The fields names and its types are extracted from the primary constructor.
Abstract types must be sealed and have at least one concrete implementation.
* No external dependencies.
* Scala map-like collections can be represented as either JSON objects or JSON arrays of pairs.
* Map keys can be of any type, even when represented as a JSON object. In that case the keys are encoded in the JSON object field names.
* When parsing an abstract type, a discriminator field to distinguish between different concrete implementations of said abstract type is needed only for those that are ambiguous: have the same amount of required fields and all of them have the same names.

_json-facile_ allows you to convert between
 * instances of arbitrary Scala data types, including parameterized and recursive algebraic data types. 
 * String JSON documents
 * JSON Abstract Syntax Trees (ASTs) with base type JsValue 
 
## Usage
_json-facile_ is really easy to use.
Just bring all relevant elements in scope with
```scala
import jsfacile.api._
```
and do one or more of the following:

1. Convert an ADT (algebraic data type) instance to JSON representation.
	```scala
	sealed trait Foo
	case class Bar(xs: Vector[String]) extends Foo
	case class Qux(i: Int, d: Option[Double]) extends Foo

	val foos = List[Foo](
		Bar(Vector("one", "two")),
		Qux(3, Some(12.3))
	)

	val json = foos.toJson
	println(json)
	```
	prints
	```json
	[{"xs":["one","two"]},{"i":3,"d":12.3}]
	```

2. Convert a JSON string document back to an ADT instance.
	```scala
	val foosParsed = json.fromJson[List[Foo]]
	println(foosParsed); // out: Right(List(Bar(Vector(one, two)), Qux(3,Some(12.3))))

	assert(Right(foos) == foosParsed) // OK
	```

3. Choose how scala maps are represented in JSON.
By default maps whose declared keys type is `Char`, `Int`, `Long`, or extends `CharSequence` are represented with JSON objects; and with JSON arrays of pairs otherwise.
	```scala
	class Key[V](val id: Long, val value: V)

	val map = Map[Key[Boolean], String](new Key(1, false) -> "one", new Key(2, true) -> "two")

	println(map.toJson)
	```
	prints
	```json
	[[{"id":1,"value":false},"one"],[{"id":2,"value":true},"two"]]
	```
	To override that behaviour put a `MapFormatDecider` instance into implicit scope.
	```scala
	implicit val mfd = new MapFormatDecider[Key[Boolean], Any, Map] {
		override val useObject: Boolean = true
	}

	println(map.toJson)
	```
	prints
	```json
	{"{\"id\":1,\"value\":false}":"one","{\"id\":2,\"value\":true}":"two"}
	```
	Note that the key values are JSON enconded in the JSON object's field names.

	CAUTION: Derived appenders (and parser) are buffered to improve speed and reduce the proyect size. Therefore, all appenders that were buffered before the declaration of the implicit instances of `MapFormatDecider` are not affected.
	To overcome this problem use the `clearAppenderBufferOf` method on the types that contains the target maps.

## More examples

1. A good example of the support of parameterized ADTs (algebraic data types) is HList.
	
	```scala
	object example1 {
		sealed abstract class HList {
			def ::[H](head: H): ::[H, this.type] = new ::(head, this)
		}
		final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
		case object Base extends HList

		def main(args: Array[String]) {
			// create an HList instance
			val hList = true :: "text" :: 3 :: Base

			// convert the HList to JSON representation
			import jsfacile.api._
			val json = hList.toJson
			println(json);

			// convert the JSON string back to an HList instance
			val parsedHList = json.fromJson[Boolean :: String :: Int :: Base.type]

			// check
			assert(parsedHList == Right(hList))
		}
	}
	```
	prints
	```json
	{"head":true,"tail":{"head":"text","tail":{"head":3,"tail":{}}}}
	```
	
2. This examples show the support of recursive data types.

	```scala
	object example2 {
		sealed trait Foo[T];
		case class FooNext[T](next: Foo[T]) extends Foo[T];
		case class FooBase[T](v: T) extends Foo[T];

		val fooBase = FooBase(7);
		val fooNext = FooNext(fooBase)

		val fooJson = fooNext.toJson
		val fooParsed = fooJson.fromJson[Foo[Int]]
		assert(Right(fooNext) == fooParsed)
	```
1. Next is a more elavorate ADTs (algebraic data types) hierarchy that includes enumerations and collections.

	```scala
	object example3 {
		object DistanceUnit extends Enumeration {
			val Meter, Millimeter = Value;
		}

		case class Distance(value: Double, unit: DistanceUnit.Value)

		sealed trait Shape
		case class Box(axis: List[Distance]) extends Shape
		case class Sphere(radius: Distance) extends Shape

		sealed trait Thing {
			def enclosingShape: Shape
			def description: String
		}
		case class Table(enclosingShape: Shape, legsAmount: Int, description: String) extends Thing
		case class Shelf(enclosingShape: Shape, levelsAmount: Option[Int], description: String) extends Thing
		case class Ball(enclosingShape: Shape, description: String) extends Thing

		def main(args: Array[String]) {
			// Create a map of things indexed by a string id.
			val thingsById = Map[String, Thing](
				"table_1" -> Table(
					legsAmount = 4,
					description = "dinner room",
					enclosingShape = Box(List(
						Distance(1.5, DistanceUnit.Meter),
						Distance(2, DistanceUnit.Meter),
						Distance(750, DistanceUnit.Millimeter)
					))
				),
				"shelf_1" -> Shelf(
					levelsAmount = Some(4),
					description = "for books",
					enclosingShape = Box(
						List(Distance(2.5, DistanceUnit.Meter),
						Distance(2, DistanceUnit.Meter),
						Distance(500, DistanceUnit.Millimeter)
					))
				),
				"ball_1" -> Ball(
					description = "soccer",
					enclosingShape = Sphere(radius = Distance(20, DistanceUnit.Millimeter))
				)
			)

			// Convert the things map to JSON representation
			import jsfacile.api._
			val json = thingsById.toJson

			// Convert the JSON string back to an algebraic data type instance
			// The type parameter is required. 
			val parsedThings = json.fromJson[Map[String, Thing]]

			// check
			assert(parsedThings == Right(thingsById))

		}
	}
	```	
## Supported standard types
* Int, Long, Float, Double, Char, Boolean, Unit, Null
* String, CharSequence
* BigInt, BigDecimal
* Option and Either (are treated specially)
* All tuples and products
* scala.{Array, Enumeration}
* scala.collection.{Iterable, Seq, IndexedSeq, LinearSeq, Set}
* scala.collection.immutable.{Iterable, Seq, IndexedSeq, LinearSeq, List, Vector, Set}
* scala.collection.mutable.{Iterable, ArrayBuffer, ListBuffer, Queue, ArrayDeque, Stack}
* scala.collection.{Map, SortedMap}
* scala.collection.immutable.{Map, HashMap, SeqMap, ListMap, SortedMap, TreeMap}
* scala.collection.mutable.{Map, HashMap, SortedMap, TreeMap}

You can add support to any other collection with a single line provided it has a factory. Look the "jsfacile.util.NonVariantHolderOf*" scala files to see how.

## Limitations

1. The automatically derived parser of abstract data types (sealed traits or abstract classes), does not support the case when two of the concrete implementations have a field with the same name but different type.

	```scala
	import jsfacile.api._
	object test3 {
		sealed trait Thing
		case class Box(lenght: Double, weight: Float)
		case class Ball(radius: String, weight: String) 

		def main(args: Array[String]): Unit = {
			val things = List[Thing](new Box(12.3, 32.1f), new Ball(45.6f, "3 kg"))
			val json = things.toJson
			println(json) // out: [{"length":12.3,"weight":32.1},{"radius":45.6,"weight":"3 kg"}]
			val thingsParsed = json.fromJson[List[Thing]]
			println(thingsParsed) // out: Left(The parsing failed at position 25 with the message: A string was expected.) 
		}
	}
	```
	This is a consequence of a design decision: execution speed is more important than support of rare or easily avoidable use cases.
2. Some IDEs hightlight false "implicit not found" kind error mensajes. Ignore or suppress them.
3. Given the `Appender`s and `Parser`s are automatically derived at compile-time, the compilation time is significantly increased.
This problem can be easily mittigated moving the involved ADTs to a separate SBT project. 

## Credits
1. To [Dymitro Mitin] who helped me innumerable times with his astonushing good answers in stackoverflow.com

###To be continued...

  [JSON]: http://json.org
  [spray]: https://github.com/spray/spray-json
  [circe]: https://circe.github.io/circe/
  [Dymitro Mitin]: https://stackoverflow.com/users/5249621/dmytro-mitin