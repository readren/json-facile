package jsfacile.jsonast

import jsfacile.api.{FromJsonStringConvertible, ToJsonConvertible}
import jsfacile.jsonast.JsonGen._
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonAstTest extends RefSpec with ScalaCheckDrivenPropertyChecks {

	object `Given an AST ...` {

		def `translating it to json and back to AST should give the original AST`(): Unit = {

			forAll { (jsValue: JsValue) =>

				val json = jsValue.toJson
				val jsParsed = json.fromJson[JsValue]
				println(s"json: $json")
				jsParsed == Right(jsValue)
			}
		}
	}
}
