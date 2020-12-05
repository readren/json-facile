# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types and String JSON documents directly, without any intermediate representation.

* No external dependencies.

* Type-class based conversion (no runtime reflection, no intrusion).

* Automatic derivation: the conversion type-classes of custom ADTs (abstract data types) are automaticaly generated at compile-time by macros. Zero boilerplate.

* An efficient JSON parser. Substantially faster than [spray] (around 84%), although considerably slower than [jsoniter] (around 33%). If the JSON contains ignored fields the difference against parsers that use intermediate representations is even greater.

* The automatic derivation works for any concrete data type. It's not required to be a case class nor inherit `scala.Product`.

	The fields names, types, and encoding order is determined by, and extracted from, the concrete type's primary constructor.
	
	Abstract types must be sealed and have at least one concrete implementation.

* Scala map-like collections can be represented as either JSON objects or JSON arrays of pairs.

* Map keys can be of any type, even when represented as a JSON object. In that case the keys are encoded in the JSON object's field names.

* No discriminator field is needed to distinguish between different concrete implementations of said abstract type, unless two of those implementations have the same amount of required fields and all of them have the same names. In that case, only the ambiguous implementations require a discriminator field. This reduces the JSON documents length considerably.

# Table of content
- [json-facile](#json-facile)
- [Table of content](#table-of-content)
	- [Usage](#usage)
		- [Convert an ADT (algebraic data type) instance to JSON representation.](#convert-an-adt-algebraic-data-type-instance-to-json-representation)
		- [Convert a JSON string document back to an ADT instance.](#convert-a-json-string-document-back-to-an-adt-instance)
		- [Choose the name of the discriminator field and if it must be appended ever or only when it's necessary.](#choose-the-name-of-the-discriminator-field-and-if-it-must-be-appended-ever-or-only-when-its-necessary)
		- [Choose how scala maps are represented in JSON.](#choose-how-scala-maps-are-represented-in-json)
		- [Implement a custom parser/appender pair.](#implement-a-custom-parserappender-pair)
	- [Why?](#why)
	- [More examples](#more-examples)
	- [Edge cases](#edge-cases)
		- [BigDecimal input limit](#bigdecimal-input-limit)
	- [Any question or idea?](#any-question-or-idea)
	- [Credits](#credits)


## Usage
_json-facile_ is really easy to use.
Just bring all relevant elements in scope with
```scala
import jsfacile.api._
```
and do one or more of the following:

### Convert an ADT (algebraic data type) instance to JSON representation.
```scala
sealed trait Foo
case class Bar(xs: Vector[String]) extends Foo
case class Qux(i: Int, d: Option[Double]) extends Foo

val foos: List[Foo] = List(
	Bar(Vector("one", "two")),
	Qux(3, Some(12.3))
)

val json: String = foos.toJson
println(json)
```
prints

```json
[{"xs":["one","two"]},{"i":3,"d":12.3}]
```

### Convert a JSON string document back to an ADT instance.
```scala
val foosParsed: Either[ParseError, List[Foo]] = json.fromJson[List[Foo]]
println(foosParsed); // out: Right(List(Bar(Vector(one, two)), Qux(3,Some(12.3))))

assert(Right(foos) == foosParsed) // OK
```
### Choose the name of the discriminator field and if it must be appended ever or only when it's necessary.

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
	val json1: String = accessories.toJson
	println(json1);

	val parsed1: Either[ParseError, List[Accessory]] = json1.fromJson[List[Accessory]]
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

val json2: String = accessories.toJson
println(json2);

val parsed2: Either[ParseError, List[Accessory]] = json2.fromJson[List[Accessory]]
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

### Choose how scala maps are represented in JSON.

By default maps whose declared keys type is `Char`, `Int`, `Long`, or extends `CharSequence` are represented with JSON objects. Otherwise the map is represented with a JSON arrays of pairs.
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

The fields order is ever determined by the primary constructor's parameters order. Therefore the keys equality remains stable.

### Implement a custom parser/appender pair.
There is no date parsers/appenders bundled in the *json-facile* library. In my opinion the most convenient way to encode dates in JSON is domain dependent.
So, suppouse the front end your scala service is communicating with is a single page application implemented with [angular](https://angular.io), and you don't need the dates be encoded in a human readable format. Then you can represent the date with the number of milliseconds since 1970 which is how the browsers represents it internally.
```scala
		implicit val instantAppender: Appender[Instant] =
			(record, instant) => record.append(instant.toEpochMilli);

		implicit val instantParser: Parser[Instant] =
			Parser[Long] ^^ Instant.ofEpochMilli

		val instant = java.time.Instant.now()
		val json = instant.toJson
		println(json);
		val parsedInstant = json.fromJson[Instant]
		assert(Right(instant) == parsedInstant)
```

## Why?
If I had known about the existence of [jsoniter], this library would not have existed. And by the time I found out, its development was too advanced to abandon it. Also, I think there are some use cases where *json-facile* is more convenient.

*json-facile* is significantly faster than all JSON libraries I know except [jsoniter] whose speed is unreachable. But they achieved that crazy speed at cost of weight and some inflexibility.
If I am not wrong, [jsoniter] allows to encode to and/or decode from `Array[byte]`, `InputStream`, and `java.nio.ByteBuffer` easily. But it's difficult to use other kind of source/sink.

With *Json-facile*, intead, it is easy to implement a custom source or sink. Just extend `jsfacile.read.AbstractCursor` for the source, and/or `jsfacile.write.Record` for the sink. The `Cursor` API was designed to minimize the amount of JSON data that needs to be holded in memory.

Other good features found in *json-facile* are:
1. Its flexibility to represent scala map-like collections. The keys may be of any type even when represented as a JSON object.
2. The easy thing is to implement custom parsers using either the `jsfacile.read.Parser` combinators or the low-level `jsfacile.read.Cursor` API.

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
	
2. This example shows the support of recursive data types.

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
3. Next is a more elavorate ADTs (algebraic data types) hierarchy that includes enumerations and collections.

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
object limitations1 {
	sealed trait Thing
	case class Box(length: Double, weight: Float) extends Thing
	case class Ball(radius: Double, weight: String)  extends Thing

	def main(args: Array[String]): Unit = {
		val things = List[Thing](new Box(12.3, 32.1f), new Ball(45.6f, "3 kg"))
		val json = things.toJson
		println(json) // out: [{"length":12.3,"weight":32.1},{"radius":45.6,"weight":"3 kg"}]
		val thingsParsed = json.fromJson[List[Thing]]
		println(thingsParsed) // compile error: could not find implicit value for parameter pt: jsfacile.api.package.Parser[List[limitations1.Thing]]
	}
}
```
The compiler error is preceded by an info message that explains the cause: * Unsupported situation while building a `Parser[limitations1.Thing]`: two implementations, `class Ball` and `class Box`, have a field with the same name ("weight") but different type.*

This limitation is a consequence of a design decision: configuration simplicity and execution speed is more important than support of rare or easily avoidable use cases.

2. The Intellij IDE hightlights false "implicit not found" kind error mensajes. Ignore or suppress them. Or use another IDE.

3. Given the `Appender`s and `Parser`s are automatically derived at compile-time, the compilation time is significantly increased.
This problem can be easily mittigated moving the involved ADTs to a separate SBT project.

4. Recursive data types are vulnerable to run-time stack overflow error when the recursion deph level of the data is high enough. This will be solved when I come up with a simple way to configure the limit.

## Edge cases
### BigDecimal input limit
The `Parser[BigDecimal]` that is bundled in the `json-facile` library is not configurable. There is no need of such thing because it is possible to override it with no much effort.
Suppouse you want to limit the size of the input number to protect your service against malicius attacks. You can achieve that with:
```scala
val BIG_DECIMAL_INPUT_MAX_LENGTH = 12

implicit val bigDecimalCustomParser: Parser[BigDecimal] = cursor => {
	val number = cursor.stringConsumedBy(Skip.jsNumber)
	if (cursor.ok) {
		if(number.length <= BIG_DECIMAL_INPUT_MAX_LENGTH) {
			BigDecimal.exact(number)
		} else {
			cursor.fail("BigDecimal input length exceeded")
			Parser.ignored[BigDecimal]
		}
	} else {
		cursor.miss("A number was expected");
		Parser.ignored[BigDecimal]
	}
}
```



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