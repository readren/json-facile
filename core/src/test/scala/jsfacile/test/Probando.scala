package jsfacile.test

import java.time.Instant

import jsfacile.api._
import jsfacile.api.builder.CoproductTranslatorsBuilder
import jsfacile.test.SampleADT.DistanceUnit.{Meter, Millimeter}
//import jsfacile.macros.Probe
import jsfacile.test.SampleADT._
//import jsfacile.test.ParserMacrosTest.{A1, Arbol, B1, C1, C2, Foo, FooBase, FooNext, Hoja, Rama, Tree}
////import jsfacile.test.SampleADT.DistanceUnit.Value

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

		////////////////////////////////////////////////

		{
			implicit val instantAppender: Appender[Instant] =
				(record, instant) => record.append(instant.toEpochMilli);

			implicit val instantParser: Parser[Instant] =
				Parser[Long] ^^ Instant.ofEpochMilli

			val instant = java.time.Instant.now()
			val json = instant.toJson
			println(json);
			val parsedInstant = json.fromJson[Instant]
			assert(Right(instant) == parsedInstant)
		}

		////////////////////////////////////////////////

		val thingBuilder = new CoproductTranslatorsBuilder[Thing]
		val ballAppendingInfoBuilder = thingBuilder.productAppendingInfoBuilder[Ball]
		ballAppendingInfoBuilder.add("radius", _.radius)
		ballAppendingInfoBuilder.add("weight", _.weight)

		thingBuilder.add[Box]
		thingBuilder.add[Ball](ballAppendingInfoBuilder.complete("ball"))

		val thingAppender = thingBuilder.appender

		implicit val ta: Appender[Thing] = thingAppender

		val list: List[Thing] = List(Box(Distance(3, Meter), 4), Ball(Distance(1, Millimeter), 2))

		val json = list.toJson
		println(json)

		////////////////////////////////////////////////

		val presentationDataJson = presentationDataOriginal.toJson
		println(presentationDataJson)
		val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
		assert(presentationDataParsed == Right(presentationDataOriginal))


	}
}