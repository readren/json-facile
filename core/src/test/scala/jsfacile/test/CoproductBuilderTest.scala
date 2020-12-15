package jsfacile.test

import jsfacile.api._
import jsfacile.test.SampleADT.{Distance, DistanceUnit}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

object CoproductBuilderTest {
	trait Thing
	case class Box(length: Distance, weight: Float) extends Thing
	case class Ball(radius: Distance, weight: Float) extends Thing
}

class CoproductBuilderTest extends RefSpec with Matchers {
	import CoproductBuilderTest._

	object `Given and data structure with a non sealed class abstract data type` {
		val things = List[Thing](Box(Distance(1.23, DistanceUnit.Meter), 32.1f), Ball(Distance(4.56, DistanceUnit.Millimeter), 3))

		val builder = new CoproductBuilder[Thing]
		builder.add[Box]
		builder.add[Ball]
		implicit val appender: Appender[Thing] = builder.appender
		implicit val parser: Parser[Thing] = builder.parser;

		val json = things.toJson

		val result = json.fromJson[List[Thing]]
		assert(result == Right(things))

	}


}
