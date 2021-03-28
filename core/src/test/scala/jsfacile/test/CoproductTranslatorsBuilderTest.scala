package jsfacile.test

import jsfacile.api._
import jsfacile.api.builder._
import jsfacile.joint.DiscriminatorDecider
import jsfacile.jsonast.{JsNull, JsNumber, JsObject}
import jsfacile.test.SampleADT.{Distance, DistanceUnit}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

object CoproductTranslatorsBuilderTest {
	trait Thing
	case class Box(length: Distance, weight: Float) extends Thing
	case class Ball(radius: Distance, weight: Float) extends Thing
	case class Block(value: Float) extends Thing
	case object One extends Thing
	case object Two extends Thing
}

class CoproductTranslatorsBuilderTest extends RefSpec with Matchers {
	import CoproductTranslatorsBuilderTest._

	object `Given a data structure with a non sealed abstract data type` {

		def `the appender and parser should work`(): Unit = {
			val things = List[Thing](Box(Distance(1.23, DistanceUnit.Meter), 32.1f), Ball(Distance(4.56, DistanceUnit.Millimeter), 3), One, Two, Block(7))

			val thingTranslatorsBuilder = new CoproductTranslatorsBuilder[Thing]
			thingTranslatorsBuilder.add[Box]()
			thingTranslatorsBuilder.add[Ball]()
			thingTranslatorsBuilder.add[One.type]()

			val twoParsingInfoBuilder = thingTranslatorsBuilder.productParsingInfoBuilder[Two.type]
			twoParsingInfoBuilder.add[Null]("two")
			val twoParsingInfo = twoParsingInfoBuilder.complete(_ => Two)

			thingTranslatorsBuilder.add[Two.type](
				ProductAppendingInfo[Two.type](Appender.convert[JsObject, Two.type](_ => JsObject("two" -> JsNull)))("two"),
				twoParsingInfo
			)

			val blockParsingInfoBuilder = thingTranslatorsBuilder.productParsingInfoBuilder[Block]
			blockParsingInfoBuilder.add[Float]("weight")
			val blockParsingInfo = blockParsingInfoBuilder.complete(args => Block(args(0).asInstanceOf[Float]))
			thingTranslatorsBuilder.add[Block](
				ProductAppendingInfo[Block](Appender.convert[JsObject, Block](x => JsObject("weight" -> JsNumber(x.value))))("weight"),
				blockParsingInfo
			)

			implicit val appender: Appender[Thing] = thingTranslatorsBuilder.appender
			implicit val parser: Parser[Thing] = thingTranslatorsBuilder.parser;

			val json = things.toJson
			println(json)

			val result = json.fromJson[List[Thing]]
			assert(result == Right(things))
		}
	}

	object `Given a structure where abstract types have abstract subtypes` {

		def `the appender and parser should work`(): Unit = {
			import ParserMacrosTest._

			val ctb = new CoproductTranslatorsBuilder[A[String]];
			ctb.add[A1[String]]()
			ctb.add[B1[String]]()
			ctb.add[C1[String]]()
			ctb.add[C2.type]()
			implicit val appender: Appender[A[String]] = ctb.appender;
			implicit val parser: Parser[A[String]] = ctb.parser;

			val set: Set[A[String]] = Set(A1("primero"), B1("dudo", 7), C1("también"), C2)

			val json = set.toJson
			println(json)

			val result = json.fromJson[Set[A[String]]]
			assert(result == Right(set))
		}

	}

	object `The coproduct translators builder examples should work` {

		def `temporal example`(): Unit = {
			import java.time._
			import java.time.temporal._

			/*
			The automatic derivation of translators (`Appender`/`Parser`) works only for algebraic data types. And most of the types of the java library are not algebraic. For instance, the `java.time.temporal.Temporal` and all its subclasses.
			If we need to translate instances of them, we have to build the translators by hand.
			An alternative would be to use a scala implementation of the `java.time` library like "https://cquiroz.github.io/scala-java-time/", whose data types are algebraic.
			But for an academic purpose, let's create translators by hand.

			Suppose our domain needs to translate instances of the abstract type `java.time.temporal.Temporal`. And suppose also, that our domain only uses two subclases of it: `Instant` and `Year`.
			Let's create the translators for `Temporal` with the help of a `CoproductTranslatorsBuilder`.
			 */
			val temporalTranslatorsBuilder = new CoproductTranslatorsBuilder[Temporal]

			/*
			To build the translators of an abstract type the builder needs to know which concrete subtypes of said abstract type it has to consider, how to discriminate them, and how to translate each of them.
			Given our domain uses only the `Instant` and `Year` subtypes of `java.time.temporal.Temporal`, it is sufficient to inform the builder about them only.
			Let's start with the `Instant` type.
			The next commented line would do all the job if the primary constructor of `Instant` included all the persistent fields.
			*/
			// builder.add[Instant];
			/*
			Because in that case all the information the builder needs about `Instant` would be obtained automatically.
			But the primary constructor of the `java.time.Instant` type is empty. So we have to supply the builder with the information it needs. The `add` method is overloaded with versions that receive said information.
			Note that the `add` method has separate parameters for the information about the appending and the parsing. This was a design decision based on the fact that in most use cases only one is needed.
			Let's construct the argument for the `parsingInfo` parameter first:
			The type of the `parsingInfo` parameter is `ProductParsingInfo` whose instances are created by the `CoproductTranslatorsBuilder.ProductParsingInfoBuilder`.
			To build a `ProductParsingInfo` instance, the `ProductParsingInfoBuilder` needs you to: (1) inform it about each persistent fields of the product (`Instant` in this case), which is done with the `add` method; and (2) inform it on how to construct an instance of the product ('Instant' in this case), which is done with the `complete` method.
			*/
			val instantParsingInfo = {
				val parsingInfoBuilder = temporalTranslatorsBuilder.productParsingInfoBuilder[Instant];
				parsingInfoBuilder.add[Long]("seconds")
				parsingInfoBuilder.add[Int]("nanos")
				parsingInfoBuilder.complete(args => Instant.ofEpochSecond(args(0).asInstanceOf[Long], args(1).asInstanceOf[Int]))
			}
			/*
			It's the turn of the appending info now:
			The `appendingInfo` parameter has a rarity in that the expression of its argument must be either: (1) a reference to a `ProductAppendingInfo` instance created by a `ProductAppendingInfoBuilder`, or (2) a literal call to the `jsfacile.api.builder.ProductAppendingInfo.apply` method.
			The first alternative is easier to use. The second is more powerful and may be slightly faster, but is open to mistakes.
			Let's show the usage of the first alternative here, for the `Instant` product. The usage of the second alternative will be shown later, for the `Year` product.
			The first alternative requires the use of a `ProductAppendingInfoBuilder` to build the `ProductAppendingInfo` instance to use as argument for the `appendingInfo` parameter. This is similar to what we did for the `parsingInfo` parameter.
			To build a `ProductAppendingInfo` instance, the `ProductAppendingInfoBuilder` needs you to inform it about each persistent field of the product (`Instant` in this case), which is done with the `add` method.
			*/
			val instantAppendingInfo = {
				val appendingInfoBuilder = temporalTranslatorsBuilder.productAppendingInfoBuilder[Instant];
				appendingInfoBuilder.add[Long]("seconds", _.getEpochSecond)
				appendingInfoBuilder.add[Int]("nanos", _.getNano)
				appendingInfoBuilder.complete;
			}
			/*
			Having all the information required by the `CoproductTranslatorsBuilder` about the `java.time.Instant` type, let's supply it.
			 */
			temporalTranslatorsBuilder.add[Instant](
				instantAppendingInfo, // ProductAppendingInfo[Instant](Appender.convert[Map[String, Long], Instant](i => Map("instant" -> i.toEpochMilli)))("instant"),
				instantParsingInfo
			)

			/*
			It's the turn of the `Year`product now.
			The parsing info construction has no difference with the one for the `Instant` product, except that the discriminator field value is specified for clarity.
			 */
			val yearParsingInfo = {
				val parsingInfoBuilder = temporalTranslatorsBuilder.productParsingInfoBuilder[Year];
				parsingInfoBuilder.add[Int]("year")
				parsingInfoBuilder.complete("Year")(args => Year.of(args(0).asInstanceOf[Int]))
			}

			/*
			The argument of the `appendingInfo` parameter for the `Year` product could be an instance constructed using the `ProductAppendingInfoBuilder` as we did with the `Instant` product. But, as promised above, here we are showing the second alternative of the expression form accepted by the `appendingInfo` parameter: a literal call to `jsfacile.api.builder.ProductAppendingInfo.apply` method.
			Said `apply` method takes an `Appender[Year]` as argument. That appender will be used by the resulting `Appender[Temporal]` when its `append` method receive an instance of `Year`. So, it must include a discriminator field if it is required by the parser on the other side, or if the field names are ambiguos (equal to the names of all the required fields of other subtype of `Temporal`).
			Given the names of the fields we chose for `Instant` and `Year` are different, there is no ambiguity, and the discriminator field is necessary only if the resulting JSON document will be read by a JSON library that requires them.
			Assuming that knowledge is centralized in an implicit `DiscriminatorDecider`, let's ask it.
			 */
			val temporalDiscriminatorDecider: DiscriminatorDecider[Temporal, CoproductsOnly] = DiscriminatorDecider.apply[Temporal, CoproductsOnly]
			/*
			Having all we need to implement the `Appender[Year]`, let's do it.
			 */
			val yearAppender = Appender[Year] { (record, year) =>
				val yearField = s""" "year":${year.getValue}"""
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
			/*
			Note that the above appender is particular for the resulting `Appender[Temporal]`.

			Having all the information about the `Year` product required by the `CoproductTranslatorsBuilder`, lets supply it.
			 */
			temporalTranslatorsBuilder.add[Year](
				ProductAppendingInfo[Year](yearAppender)("year"),
				yearParsingInfo
			)

			/*
			The information about the two subtypes of `Temporal` used in our domain were supplied to the builder. So, it is prepared to build the `Temporal` translators.
			 */
			implicit val temporalAppender: Appender[Temporal] = temporalTranslatorsBuilder.appender;
			implicit val temporalParser: Parser[Temporal] = temporalTranslatorsBuilder.parser;

			val set = Set[Temporal](Instant.now, Year.now)

			val json = set.toJson
			println(json)
			val result = json.fromJson[Set[Temporal]]
			assert(result == Right(set))
		}
	}
}
