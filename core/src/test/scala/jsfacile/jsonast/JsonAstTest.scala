package jsfacile.jsonast

import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonAstTest extends RefSpec with ScalaCheckDrivenPropertyChecks {

	object `Given an AST ...` {
		import jsfacile.api._
		import JsonGen._

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
