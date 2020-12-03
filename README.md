# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types and String JSON documents directly, without any intermediate representation.
* No external dependencies.
* Type-class based conversion (no runtime reflection, no intrusion).
* Automatic derivation: the conversion type-classes of custom ADTs (abstract data types) are automaticaly generated at compile-time by macros. Zero boilerplate.
* An efficient JSON parser. Substantially faster than [spray] (around 84%), although considerably slower than [jsoniter] (around 33%). If the JSON contains ignored fields the difference against parsers that use intermediate representations is even greater.
* The automatic derivation works for any concrete data type. It's not required to be a case class nor inherit `scala.Product`. The fields names, types, and encoding order is determined by and extracted from the concrete type's primary constructor.
Abstract types must be sealed and have at least one concrete implementation.
* Scala map-like collections can be represented as either JSON objects or JSON arrays of pairs.
* Map keys can be of any type, even when represented as a JSON object. In that case the keys are encoded in the JSON object field names.
* When parsing an abstract type, no discriminator field is needed to distinguish between different concrete implementations of said abstract type, unless two of those implementations have the same amount of required fields and all of them have the same names. In that case, only the ambiguous implementations require a discriminator field. This reduces the JSON documents length considerably.

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
3. Choose the name of the discriminator field and if it must be appended ever or only when it's necessary.
By default the discriminator field name is the question mark (`fieldName="?"`) and it's appended only when necessary (`required=false`). 
	```scala
	import jsfacile.api._

	sealed trait Accessory {
		def description: String
	}
	case class Mouse(description: String, wireless: Boolean) extends Accessory
	case class Keyboard(description: String, wireless: Boolean, hasNumPad: Boolean) extends Accessory

	val accessories: List[Accessory] = List(
		Mouse("small", wireless = false),
		Keyboard("cheap", wireless = true, hasNumPad = false)
	)

	{
		val json1 = accessories.toJson
		println(json1);

		val parsed1 = json1.fromJson[List[Accessory]]
		assert(parsed1 == Right(accessories))
	}
	```
	prints
	```json
	[{"description":"small","wireless":false},{"description":"cheap","wireless":true,"hasNumPad":false}]
	```
	There are two ways to change the default behaviour: having an instance of the `jsfacile.api.DiscriminatorDecider` type-class in the implicit scope; or annotating the abstract type with the `jsfacile.api.discriminatorField` annotation. The second has precedense over the first. The first is inherited and the second not.

	```scala
	implicit val accessoryDiscriminatorDecider = new DiscriminatorDecider[Accessory] {
		override def fieldName: String = "type"
		override def required: Boolean = true
	}

	val json2 = accessories.toJson
	println(json2);

	val parsed2 = json2.fromJson[List[Accessory]]
	assert(parsed2 == Right(accessories))
	```
	prints
	```json
	[{"type":"Mouse","description":"small","wireless":false},{"type":"Keyboard","description":"cheap","wireless":true,"hasNumPad":false}]
	```

	The same result is acchieved with the annotation approach:

	```scala
	@discriminatorField("type", true) sealed trait Accessory {
		def description: String
	}
	```

4. Choose how scala maps are represented in JSON.
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
	Note that the keys are JSON enconded in the JSON object's field names.
	The fields order is ever determined by the primary constructor's parameters order. 

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

## Any question or idea?
Fell free to ask question in [chat](https://gitter.im/json-facile/community#), open issues, or contribute by creating pull requests.

## Credits
1. To [Dymitro Mitin] who helped me innumerable times with his astonushing good answers in stackoverflow.com

###To be continued...

  [JSON]: http://json.org
  [spray]: https://github.com/spray/spray-json
  [circe]: https://circe.github.io/circe/
  [jsoniter]: https://github.com/plokhotnyuk/jsoniter-scala
  [Dymitro Mitin]: https://stackoverflow.com/users/5249621/dmytro-mitin