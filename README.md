# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types and String JSON documents directly, without any intermediate representations.
* An efficient JSON parser. Sligtly Faster than [spray].
* Type-class based conversion (no runtime reflection, no intrusion).
* Automatic derivation: the conversion type classes are automaticaly generated at compile-time by macros. Zero boilerplate. 
* No external dependencies.
* A discriminator to differentiate between products of a coproduct is needed by the parser only when two products have the same amount of required fields and all these fields have the same names.
* Scala maps can be represented as both, JSON object or as a JSON array of pairs
* Map keys can be of any type, even when represented as a JSON object. In that case the keys are encoded in the JSON object field names.

_json-facile_ allows you to convert between
 * instances of arbitrary Scala types including parameterized algebraic data types 
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
	``
	Note that the key values are JSON enconded in the JSON object's field names.
## More examples

1. A more elavorate ADT (algebraic data type) that includes enumerations and collections.

	```scala
	object example1 {
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
	
2. A good example of the support of parameterized ADTs are HLists.
	
	```scala
	object example2 {
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
	
3. Convert a recursive ADT to JSON and back

	```scala
	object example3 {
		sealed trait Foo[T];
		case class FooNext[T](next: Foo[T]) extends Foo[T];
		case class FooBase[T](v: T) extends Foo[T];

		val fooBase = FooBase(7);
		val fooNext = FooNext(fooBase)

		val fooJson = fooNext.toJson
		val fooParsed = fooJson.fromJson[Foo[Int]]
		assert(Right(fooNext) == fooParsed)
	```
	
## Supported standard types

* Int, Long, Float, Double, Char, Boolean, Unit, Null
* String, CharSequence
* BigInt, BigDecimal
* Option (is treated specially)
* All tuples and products
* scala.{Array, Enumeration}
* scala.collection.{Iterable, Seq, IndexedSeq, LinearSeq, Set}
* scala.collection.immutable.{Iterable, Seq, IndexedSeq, LinearSeq, List, Vector, Set}
* scala.collection.mutable.{Iterable, ArrayBuffer, ListBuffer, Queue, ArrayDeque, Stack}
* scala.collection.{Map, SortedMap}
* scala.collection.immutable.{Map, HashMap, SeqMap, ListMap, SortedMap, TreeMap}
* scala.collection.mutable.{Map, HashMap, SortedMap, TreeMap}
* JsValue

You can add support to any other collection with a single line. Look the "jsfacile.util.NonVariantHolderOf*" scala files to see how.

## Limitations

1. The automatically derived parser of abstract types (sealed traits or abstract classes), does not support the case when two of the concrete implementations have a field with the same name but different type.

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
This problem can be easily mittigated moving the involved ADTs to separate SBT project. 

###To be continued...

  [JSON]: http://json.org
  [spray]: https://github.com/spray/spray-json
