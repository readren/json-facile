package jsfacile.test

import java.time.Instant
import jsfacile.test.SampleADT._
import jsfacile.test.SampleADT.DistanceUnit._


object Probando { // Internal error: unable to find the outer accessor symbol of object App

	trait Thing
	case class Box(length: Distance, weight: Float) extends Thing
	case class Ball(radius: Distance, weight: Float) extends Thing
	case object Zero extends Thing
	case object One extends Thing
	case object Two extends Thing

	//	object DistanceUnit extends Enumeration {
	//		val Meter, Millimeter = Value;
	//	}
	//	case class Distance(value: Double, unit: DistanceUnit.Value)
	//
	//
	//	trait Thing
	//	case class Box(length: Distance, weight: Float) extends Thing
	//	case class Ball(radius: Distance, weight: Float) extends Thing

	def main(args: Array[String]): Unit = {

		///////////////

		{
			import jsfacile.api._
			sealed trait Foo
			case class Bar(xs: Vector[String]) extends Foo
			case class Qux(i: Int, d: Option[Double]) extends Foo

			val foos: List[Foo] = List(
				Bar(Vector("one", "two")),
				Qux(3, Some(12.3))
			)

			// Convert the ADT to JsDocument
			val jsonDoc: AppendResult = foos.toJson
			println(jsonDoc.value)

		}

		{
			import jsfacile.api._

			implicit val dd = new DiscriminatorDecider[Zero.type, AnyAdt] {
				override def fieldName: String = "tipo"
				override def required: Boolean = true
			}

			println(Zero.toJson.value)
		}

		{
			import jsfacile.api._

			@discriminatorField("tio", true)
			sealed trait Accessory {
				def description: String
			}
			case class Mouse(description: String, wireless: Boolean) extends Accessory
			case class Keyboard(description: String, wireless: Boolean, hasNumPad: Boolean) extends Accessory
			case class Joystick(description: String, wireless: Boolean) extends Accessory
			object Pin extends Accessory {
				override def description = "pin";
			}

			println(Pin.toJsonAs[Accessory])

			implicit def accessoryDiscriminatorDecider[A <: Accessory] = new DiscriminatorDecider[A, AnyAdt] {
				override def fieldName: String = "type"
				override def required: Boolean = true
			}

			println(Pin.toJsonAs[Accessory])

			println(Keyboard("cheap", wireless = true, hasNumPad = false).toJson)

		}

		{
			import jsfacile.api._

			implicit val thingsDiscriminatorDecider = new DiscriminatorDecider[Box, ProductsOnly] {
				override def fieldName: String = "thing"
				override def required: Boolean = false
			}
			implicit val discValueMapper = new DiscriminatorValueMapper[Box, AnyAdt] {
				override def apply(symbolName: String): String = symbolName.substring(0, 2)
			}

			val box = Box(Distance(3, Meter), 4)
			val boxJson = box.toJson.value;
			println("boxJson:" + boxJson)
			val boxParsed = boxJson.fromJson[Box]
			println("boxParsed:" + boxParsed)
		}

		{
			import jsfacile.api._

			case class Foo(id: Int, name: String)

			val aJsDocument: AppendResult = Foo(40, "foo").toJsDocument
			println(aJsDocument)

			case class MixedDto(id: Int, name: String, jsonData: JsDocument)

			val mixedDto = MixedDto(123, "John Galt", new JsDocument("""{"age": 40, "isRebel": true, "infractions":["has self-esteem","is intransigent"]}"""))
			println("mixedDto: " + mixedDto)
			val json = mixedDto.toJson.value
			println("json: " + json)
			val parsed = json.fromJson[MixedDto]
			println("parsed: " + parsed)
			assert(Right(mixedDto) == parsed, json)

			import jsfacile.jsonast._

			val aJsValue: JsValue = JsObject("id" -> JsNumber(3), "data" -> JsDocument("""[true, "hello"]"""))
			println(aJsValue.toJson)

		}

		////////////////////////////////////////////////

		{
			import jsfacile.api._

			implicit val instantAppender: Appender[Instant] =
				(record, instant) => record.append(instant.toEpochMilli);

			implicit val instantParser: Parser[Instant] =
				Parser[Long] ^^ Instant.ofEpochMilli

			val instant = java.time.Instant.now()
			val json = instant.toJson.value
			println(json);
			val parsedInstant = json.fromJson[Instant]
			assert(Right(instant) == parsedInstant)
		}

		////////////////////////////////////////////////

		{
			import jsfacile.api._
			import jsfacile.api.builder._

			val thingBuilder = new CoproductTranslatorsBuilder[Thing]
			val ballAppendingInfoBuilder = thingBuilder.productAppendingInfoBuilder[Ball]
			ballAppendingInfoBuilder.add("radius", _.radius)
			ballAppendingInfoBuilder.add("weight", _.weight)

			thingBuilder.add[Box]
			thingBuilder.add[Ball](ballAppendingInfoBuilder.complete("ball"))

			val thingAppender = thingBuilder.appender

			implicit val ta: Appender[Thing] = thingAppender

			val list: List[Thing] = List(Box(Distance(3, Meter), 4), Ball(Distance(1, Millimeter), 2))

			val json = list.toJson.value
			println(json)

			////////////////////////////////////////////////

			val presentationDataJson = presentationDataOriginal.toJson.value
			println(presentationDataJson)
			val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
			assert(presentationDataParsed == Right(presentationDataOriginal))

		}

	}
}