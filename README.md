# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types (ADTs) and JSON documents directly, without any intermediate representation.

* No external dependencies.

* Type-class based conversion (no runtime reflection, no intrusion).

* Automatic derivation: the conversion type-classes of custom ADTs (algebraic data types) are automatically generated at compile-time by macros. Zero boilerplate.

* An efficient JSON parser. Substantially faster than [spray] (around 80%), although considerably slower than [jsoniter] (around 35%). If the JSON contains ignored fields the difference against parsers that use intermediate representations is even greater.

* The automatic derivation works also for non-algebraic data types if the primary constructor contains all the persistent fields. It's not required to be a case class nor inherit `scala.Product`.

	The fields names, types, and encoding order is determined by, and extracted from, the concrete type's primary constructor.
	
	Abstract ADTs must have at least one concrete ADT. 

* Non algebraic abstract types are supported with the help of a builder.

* Scala map-like collections can be represented as either JSON objects or JSON arrays of pairs.

* Map keys can be of any type, even when represented as a JSON object. In that case the keys are encoded in the JSON object's field names.

* No discriminator field is needed to distinguish between different concrete subtypes of an abstract type, unless two of those subtypes have the same amount of required fields and all of them have the same names. In that case, only the ambiguous subtypes representations require a discriminator field. This reduces the JSON documents length considerably.

# Table of content
- [json-facile](#json-facile)
- [Table of content](#table-of-content)
	- [Installation](#installation)
	- [Usage](#usage)
		- [Convert an ADT (algebraic data type) instance to JSON representation.](#convert-an-adt-algebraic-data-type-instance-to-json-representation)
		- [Convert a JSON string document back to an ADT instance.](#convert-a-json-string-document-back-to-an-adt-instance)
		- [Choose the name of the discriminator field and if it must be appended always or only when it's necessary.](#choose-the-name-of-the-discriminator-field-and-if-it-must-be-appended-always-or-only-when-its-necessary)
		- [Choose how scala maps are represented in JSON.](#choose-how-scala-maps-are-represented-in-json)
		- [Build a translator for a type defined with a non-sealed trait.](#build-a-translator-for-a-type-defined-with-a-non-sealed-trait)
		- [Build the translators for a non-algebraic concrete data type.](#build-the-translators-for-a-non-algebraic-concrete-data-type)
		- [Build the translators for a non-algebraic abstract data type.](#build-the-translators-for-a-non-algebraic-abstract-data-type)
	- [Why?](#why)
	- [More examples](#more-examples)
	- [Edge cases](#edge-cases)
		- [BigDecimal input limit](#bigdecimal-input-limit)
	- [Any question or idea?](#any-question-or-idea)
	- [Credits](#credits)


## Installation
There are still no `json-facile` artifacts published in a public repository (like maven). So you have to create them locally.

1- Make a copy of the `json-facile` library to your machine, either cloning the whole repository
```
git clone https://github.com/readren/json-facile.git
```
or downloading and uncompressing the [zip with the last version](https://github.com/readren/json-facile/archive/master.zip).

2- Navigate to the folder where you put it and run `sbt publishLocal`.

Having the `json-facile` artifacts in your local Ivy repository, you may add the dependencies to them.

The library is separated in two artifacts because one is needed at compile time only.
Add the `core` artifact with a "compile" scope and the `macros` artifact with "compile-internal" or "provided" scope to your list of dependencies:

```scala
libraryDependencies ++= Seq(
	"org.readren.json-facile" %% "core" % "0.3.1-SNAPSHOT",
	"org.readren.json-facile" %% "macros" % "0.3.1-SNAPSHOT" % "compile-internal"
)
```

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
### Choose the name of the discriminator field and if it must be appended always or only when it's necessary.

By default, the discriminator field name is the question mark (`fieldName="?"`) and it's appended only when necessary (`required=false`). 
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
@jsfacile.annotations.discriminatorField("type", true) sealed trait Accessory {
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
Note that the keys are JSON encoded in the JSON object's field names.

The fields order is ever determined by the primary constructor's parameters order. Therefore, the keys' equality remains stable.

### Build a translator for a type defined with a non-sealed trait.
The automatic derivation of translators (`Appender`/`Parser`) works for algebraic data types; and a type defined with a non-sealed trait is not algebraic.

In this case the translators have to be done manually with the help of the `CoproductTranslatorBuilder`.

The following example shows how to build the translators for an abstract data type whose unique missing part to be algebraic is the `sealed` keyword. That is the case for type hierarchies that are spread along many files.

```scala
import jsfacile.api._
import jsfacile.api.builder._

trait Thing // Note that `Thing` is not sealed.
case class Box(length: Double, weight: Float) extends Thing
case class Ball(radius: Double, weight: Float) extends Thing

// create the builder for the `Thing` type.
val builder = new CoproductTranslatorsBuilder[Thing]
// specify which are the subtypes of `Thing` that will be considered by the resulting appender/parser.
builder.add[Box]
builder.add[Ball]

// create the appender/parser of `Thing`.
implicit val thingAppender: Appender[Thing] = builder.appender
implicit val thingParser: Parser[Thing] = builder.parser;

// a list of `Thing` instances fort testing
val things = List[Thing](
	new Box(1.23, 32.1f),
	new Ball(4.56, 3)
)
val json = things.toJson
println(json)

val result = json.fromJson[List[Thing]]
assert(result == Right(things))
```
prints
```json
[{"length":1.23,"weight":32.1},{"radius":4.56,"weight":3.0}]
```


### Build the translators for a non-algebraic concrete data type.
The automatic derivation relies on the primary constructor having all non-transient fields of the data type. That is not the case for much java standard library's classes. For example `java.time.Instant`.

There is no `java.time` parsers/appenders bundled in the *json-facile* library because, in my opinion, the most convenient way to encode temporals in JSON is domain dependent.

So, suppose the front end your scala service is communicating with is a single page application, and you don't need the `Instant` values be JSON-represented in a human-readable format. Then you can encode them in a JSON number with the milliseconds since 1970, which is how the browsers represents it internally.
```scala
import java.time.Instant
import jsfacile.api._
import jsfacile.api.builder._

// Implement an [[Appender]] of [[Instant]] and put an instance of it in the implicit scope.
implicit val instantAppender: Appender[Instant] =
	(record, instant) => record.append(instant.toEpochMilli)

// Implement a [[Parser]] of [[Instant]] and put an instance of it in the implicit scope.
implicit val instantParser: Parser[Instant] =
	Parser[Long] ^^ Instant.ofEpochMilli

val instant = java.time.Instant.now()
val json = instant.toJson // this uses the `instanceAppender` 
println(json)
val parsedInstant = json.fromJson[Instant] // this uses the `instantParser`
assert(Right(instant) == parsedInstant)
```

### Build the translators for a non-algebraic abstract data type.
The automatic derivation of translators (`Appender`/`Parser`) works only for algebraic data types. And most of the types of the java library are not algebraic. For instance, the `java.time.temporal.Temporal` and all its subclasses.
If we need to translate instances of them, we have to build the translators by hand.

An alternative would be to use a scala implementation of the `java.time` library like [scala-java-time](https://cquiroz.github.io/scala-java-time/), whose data types are algebraic.
For an academic sake, let's create translators by hand.

Suppose our domain needs to translate instances of the abstract type `java.time.temporal.Temporal`. Suppose also, that our domain only uses two subclases of it: `Instant` and `Year`.
Let's create the translators for `Temporal` with the help of a `CoproductTranslatorsBuilder`.
```scala
import java.time._
import java.time.temporal._
import jsfacile.api._
import jsfacile.api.builder._

val temporalTranslatorsBuilder = new CoproductTranslatorsBuilder[Temporal];
```
To build the translators of an abstract type the builder needs to know which concrete subtypes of said abstract type it has to consider, how to discriminate them, and how to translate each of them.
Given our domain uses only the `Instant` and `Year` subtypes of `java.time.temporal.Temporal`, it is sufficient to inform the builder about them only.

Let's start with the `Instant` type.
The next commented line would do all the job if the primary constructor of `Instant` included all the persistent fields.
```scala
// temporalTranslatorsBuilder.add[Instant]
```
Because in that case all the information the builder needs about `Instant` would be obtained automatically.
But the primary constructor of the `java.time.Instant` type is empty. So we have to supply the builder with the information it needs. The `add` method is overloaded with versions that receive said information.

Note that the `add` method has separated parameters for the information about the appending and the parsing. This was a design decision based on the fact that in most use cases only one is needed.

Let's construct the argument for the `parsingInfo` parameter first:
The type of the `parsingInfo` parameter is `ProductParsingInfo` whose instances are created by the `CoproductTranslatorsBuilder.ProductParsingInfoBuilder`.

To build a `ProductParsingInfo` instance, the `ProductParsingInfoBuilder` needs you to: (1) inform it about each persistent fields of the product (`Instant` in this case), which is done with the `add` method; and (2) inform it on how to construct an instance of the product ('Instant' in this case), which is done with the `complete` method.
```scala
val instantParsingInfo = {
	val parsingInfoBuilder = temporalTranslatorsBuilder.productParsingInfoBuilder[Instant];
	parsingInfoBuilder.add[Long]("seconds");
	parsingInfoBuilder.add[Int]("nanos");
	parsingInfoBuilder.complete(args => Instant.ofEpochSecond(args(0).asInstanceOf[Long], args(1).asInstanceOf[Int]))
}
```
It's the turn of the appending info now:
The `appendingInfo` parameter has a rarity in that the expression of its argument must be either: (1) a reference to a `ProductAppendingInfo` instance created by a `ProductAppendingInfoBuilder`, or (2) a literal call to the `jsfacile.api.builder.ProductAppendingInfo.apply` method.

The first alternative is easier to use. The second is more powerful and may be slightly faster, but is open to mistakes.

Let's show the usage of the first alternative here, for the `Instant` product. The usage of the second alternative will be shown later, for the `Year` product.

The first alternative requires the use of a `ProductAppendingInfoBuilder` to build the `ProductAppendingInfo` instance to use as argument for the `appendingInfo` parameter. This is similar to what we did for the `parsingInfo` parameter.

To build a `ProductAppendingInfo` instance, the `ProductAppendingInfoBuilder` needs you to inform it about each persistent field of the product (`Instant` in this case), which is done with the `add` method.
```scala
val instantAppendingInfo = {
	val appendingInfoBuilder = temporalTranslatorsBuilder.productAppendingInfoBuilder[Instant];
	appendingInfoBuilder.add[Long]("seconds", _.getEpochSecond);
	appendingInfoBuilder.add[Int]("nanos", _.getNano);
	appendingInfoBuilder.complete
}
```
Having all the information required by the `CoproductTranslatorsBuilder` about the `java.time.Instant` type, let's supply it.
```scala
temporalTranslatorsBuilder.add[Instant](
	instantAppendingInfo,
	instantParsingInfo
)
```
It's the turn of the `Year`product now.
The parsing info construction has no difference with the one for the `Instant` product, except that the discriminator field value is specified for clarity.
```scala
val yearParsingInfo = {
	val parsingInfoBuilder = temporalTranslatorsBuilder.productParsingInfoBuilder[Year];
	parsingInfoBuilder.add[Int]("year");
	parsingInfoBuilder.complete("Year")(args => Year.of(args(0).asInstanceOf[Int]))
}
```
The argument of the `appendingInfo` parameter for the `Year` product could be an instance constructed using the `ProductAppendingInfoBuilder` as we did with the `Instant` product. But, as promised above, here we are showing the second alternative of the expression form accepted by the `appendingInfo` parameter: a literal call to `jsfacile.api.builder.ProductAppendingInfo.apply` method.

Said `apply` method takes an `Appender[Year]` as argument. That appender will be used by the resulting `Appender[Temporal]` when its `append` method receive an instance of `Year`. So, it must include a discriminator field if it is required by the parser on the other side, or if the field names are ambiguos (equal to the names of all the required fields of other subtype of `Temporal`).

Given the names of the fields we chose for `Instant` and `Year` are different, there is no ambiguity, and the discriminator field is necessary only if the resulting JSON document will be read by a JSON library that requires them.

Assuming that knowledge is centralized in an implicit `DiscriminatorDecider`, let's ask it.
```scala
val temporalDiscriminatorDecider: DiscriminatorDecider[Temporal] = implicitly[DiscriminatorDecider[Temporal]];
```
Having all we need to implement the `Appender[Year]`, let's do it.
```scala
val yearAppender = Appender[Year] { (record, year) =>
	val yearField = s""" "year":${year.getValue}""";
	if(temporalDiscriminatorDecider.required) {
		record.append(
			s"""
				|"{${temporalDiscriminatorDecider.fieldName}":"Year",
				|$yearField
				|""".stripMargin)
	} else {
		record.append(s"""{$yearField}""")
	}
}
```
Note that the above appender is particular for the resulting `Appender[Temporal]`.

Having all the information about the `Year` product required by the `CoproductTranslatorsBuilder`, lets supply it.
```scala
temporalTranslatorsBuilder.add[Year](
	ProductAppendingInfo[Year](yearAppender)("year"),
	yearParsingInfo
)
```
The information about the two subtypes of `Temporal` used in our domain were supplied to the builder. So, it is prepared to build the `Temporal` translators.
```scala
implicit val temporalAppender: Appender[Temporal] = temporalTranslatorsBuilder.appender;
implicit val temporalParser: Parser[Temporal] = temporalTranslatorsBuilder.parser;

val set = Set[Temporal](Instant.now, Year.now);

val json = set.toJson;
println(json);
val result = json.fromJson[Set[Temporal]];
assert(result == Right(set));
```
prints something like
```json
[{"seconds":1611864986,"nanos":882000000},{ "year":2021}]
```

## Why?
If I had known about the existence of [jsoniter], this library would not have existed. And by the time I found out, its development was too advanced to abandon it. Also, I think there are some use cases where *json-facile* is more convenient.

*json-facile* is significantly faster than all JSON libraries I know except [jsoniter] whose speed is unreachable. But they achieved that crazy speed at cost of weight and some inflexibility.
If I am not wrong, [jsoniter] allows encoding to and/or decoding from `Array[byte]`, `InputStream`, and `java.nio.ByteBuffer` easily; but it's difficult to use another kind of source/sink.

With *Json-facile*, instead, it is easy to implement a custom source or sink. Just extend `jsfacile.read.AbstractCursor` for the source, and/or `jsfacile.write.Record` for the sink. The `Cursor` API was designed to minimize the amount of JSON data that needs to be holded in memory.

Other good features found in *json-facile* are:
1. Its flexibility to represent map-like collections. The keys may be of any type even when represented as a JSON object.
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
* Byte, Short, Int, Long, Float, Double, Char, Boolean, Unit, Null, Nothing
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
		println(thingsParsed) // compile error: Unable to derive a parser for `List[jsfacile.test.Probando.Thing]` because it depends on the parser for `jsfacile.test.Probando.Thing` whose derivation has failed saying: Unsupported situation while building a `Parser[jsfacile.test.Probando.Thing]`: two implementations, `class Ball` and `class Box`, have a field with the same name ("weight") but different type.
	}
}
```
This limitation is a consequence of a design decision: configuration simplicity and execution speed is more important than support of rare or easily avoidable use cases.

Note that the appender does not complain. Only the parser. This is intended because the generated JSON document may be parsed by other JSON library that supports namesakes fields with differnt types. 

2. Given the `Appender`s and `Parser`s are automatically derived at compile-time, the compilation time suffers an increase proportional to the amount of derivations.
This problem can be easily mittigated moving the involved ADTs to a separate SBT project.

3. Recursive data types are vulnerable to run-time stack overflow error when the recursion deph level of the data is high enough. This will be solved when I come up with a simple way to configure the limit.

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
1. To [Dymitro Mitin] who helped me innumerable times with his astonishing good answers in stackoverflow.com

###To be continued...

  [JSON]: http://json.org
  [SBT]: https://www.scala-sbt.org/
  [spray]: https://github.com/spray/spray-json
  [circe]: https://circe.github.io/circe/
  [jsoniter]: https://github.com/plokhotnyuk/jsoniter-scala
  [Dymitro Mitin]: https://stackoverflow.com/users/5249621/dmytro-mitin