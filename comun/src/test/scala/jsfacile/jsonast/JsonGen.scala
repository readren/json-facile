package jsfacile.jsonast

/**
 * Generator of JSONType objects with a given tree depth
 */
import org.scalacheck.Gen._
import org.scalacheck._

trait JsonGen {

	implicit def arbitraryJsValue: Arbitrary[JsValue] =
		Arbitrary(sized { depth =>
			genJsValue(depth)
		})

	implicit def arbitraryJsObject: Arbitrary[JsObject] =
		Arbitrary(sized { depth =>
			genJsObject(depth)
		})

	implicit def arbitraryJsArray: Arbitrary[JsArray] =
		Arbitrary(sized { depth =>
			genJsArray(depth)
		})

	/** generate either a JSONArray or a JSONObject */
	def genJsValue(depth: Int): Gen[JsValue] = frequency(
		depth -> genJsArray((depth * 3)/ 4),
		depth -> genJsObject((depth * 3)/ 4),
		39 -> genJsNumber,
		39 -> genJsString,
		19 -> genJsBoolean,
		10 -> genJsNull
	)

	def genJsArray(depth: Int): Gen[JsArray] = for {
		n <- choose(0, 9)
		vals <- genListOfValues(n, depth)
	} yield JsArray(vals)

	def genJsObject(depth: Int): Gen[JsObject] = for {
		n <- choose(1, 9)
		ks <- getListOfKeys(n)
		vals <- genListOfValues(n, depth)
	} yield JsObject(Map((ks zip vals): _*))

	def getListOfKeys(n: Int): Gen[List[String]] = listOfN(n, genString)

	def genListOfValues(n: Int, depth: Int): Gen[List[JsValue]] = listOfN(n, genJsValue(depth))

	def genJsNull: Gen[JsNull.type] = Gen.const(JsNull)

	def genJsBoolean: Gen[JsBoolean] = for {b <- Arbitrary.arbitrary[Boolean]} yield JsBoolean(b)

	def genJsNumber: Gen[JsNumber] = for (bd <- Arbitrary.arbitrary[BigDecimal]) yield JsNumber(bd)

	def genJsString: Gen[JsString] = for {str <- genString} yield JsString(str);

	def genString: Gen[String] = for {
		list <- listOf(choose[Int](0, 0x10FFFF))
	} yield {
		val sb = new java.lang.StringBuilder(list.length * 2)
		list.foreach(sb.appendCodePoint)
		sb.toString
	}

}
/** import the members of that object to use the implicit arbitrary[JSONType] */
object JsonGen extends JsonGen
