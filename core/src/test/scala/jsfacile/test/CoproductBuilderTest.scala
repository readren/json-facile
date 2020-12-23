package jsfacile.test

import jsfacile.api._
import jsfacile.api.builder._
import jsfacile.jsonast.{JsArray, JsFalse, JsNull, JsNumber, JsObject, JsTrue}
import jsfacile.read.BasicParsers
import jsfacile.test.SampleADT.{Distance, DistanceUnit}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

object CoproductBuilderTest {
	trait Thing
	case class Box(length: Distance, weight: Float) extends Thing
	case class Ball(radius: Distance, weight: Float) extends Thing
	case object Zero extends Thing
	case object One extends Thing
	case object Two extends Thing
}

class CoproductBuilderTest extends RefSpec with Matchers {
	import CoproductBuilderTest._

	object `Given a data structure with a non sealed abstract data type` {

		def `the appender and parser should work`(): Unit = {
			val things = List[Thing](Box(Distance(1.23, DistanceUnit.Meter), 32.1f), Ball(Distance(4.56, DistanceUnit.Millimeter), 3), Zero, One, Two)

			val thingBuilder = new CoproductBuilder[Thing]
			thingBuilder.add[Box]
			thingBuilder.add[Ball]
			thingBuilder.add[Zero.type]
			thingBuilder.add[One.type]

			val twoParsingInfoBuilder = thingBuilder.productParsingInfoBuilder[Two.type]
			twoParsingInfoBuilder.add[Null]("two", None)
			val twoParsingInfo = twoParsingInfoBuilder.complete("Two", _ => Two)

			thingBuilder.add[Two.type](
				ProductAppendingInfo[Two.type](Appender[JsObject].map(_ => new JsObject("two" -> JsNull)))("two"),
				twoParsingInfo
			)

			implicit val appender: Appender[Thing] = thingBuilder.appender
			implicit val parser: Parser[Thing] = thingBuilder.parser;

			val json = things.toJson
			println(json)

			val result = json.fromJson[List[Thing]]
			assert(result == Right(things))
		}
	}

	object `Given a structure where abstract types have abstract subtypes` {

		def `the appender and parser should work`(): Unit = {
			import ParserMacrosTest._

			val set: Set[A[String]] = Set(A1("primero"), B1("dudo", 7), C1("también"), C2)

			val cb = new CoproductBuilder[A[String]];
			cb.add[A1[String]]
			cb.add[B1[String]]
			cb.add[C1[String]]
			cb.add[C2.type]
			implicit val appender: Appender[A[String]] = cb.appender;
			implicit val parser: Parser[A[String]] = cb.parser;

			val json = set.toJson
			println(json)

			val result = json.fromJson[Set[A[String]]]
			assert(result == Right(set))
		}

	}
}
