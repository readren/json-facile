package read

/**
 * Generator of JSONType objects with a given tree depth
 */
import org.scalacheck.Gen._
import org.scalacheck._
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

trait JsonGen {

	implicit def arbitraryJsValue: Arbitrary[JsValue] =
		Arbitrary {sized(depth => jsonType(depth))}

	implicit def arbitraryJsObject: Arbitrary[JsObject] =
		Arbitrary {sized(depth => jsonObject(depth))}

	implicit def arbitraryJsArray: Arbitrary[JsArray] =
		Arbitrary {sized(depth => jsonArray(depth))}

	/** generate either a JSONArray or a JSONObject */
	def jsonType(depth: Int): Gen[JsValue] = oneOf(jsonArray(depth), jsonObject(depth), jsNumber, jsString, jsBoolean, jsNull)

	/** generate a JSONArray */
	def jsonArray(depth: Int): Gen[JsArray] = for {
		n <- choose(1, 4)
		vals <- values(n, depth)
	} yield JsArray(vals)

	/** generate a JSONObject */
	def jsonObject(depth: Int): Gen[JsObject] = for {
		n <- choose(1, 4)
		ks <- keys(n)
		vals <- values(n, depth)
	} yield JsObject(Map((ks zip vals): _*))

	/** generate a list of keys to be used in the map of a JSONObject */
	def keys(n: Int): Gen[List[String]] = listOfN(n, cadena)

	/**
	 * generate a list of values to be used in the map of a JSONObject or in the list
	 * of a JSONArray.
	 */
	def values(n: Int, depth: Int): Gen[List[JsValue]] = listOfN(n, jsValue(depth))

	/**
	 * generate a value to be used in the map of a JSONObject or in the list
	 * of a JSONArray.
	 */
	def jsValue(depth: Int): Gen[JsValue] =
		if (depth == 0)
			terminalType
		else
			oneOf(jsonType(depth - 1), terminalType)

	/** generate a terminal value type */
	def terminalType: Gen[JsValue] = oneOf(jsNumber, jsString, jsBoolean, jsNull)

	def jsNull: Gen[JsNull.type] = Gen.const(JsNull)

	def jsBoolean: Gen[JsBoolean] = for {b <- Arbitrary.arbitrary[Boolean]} yield JsBoolean(b)

	def jsNumber: Gen[JsNumber] = for (bd <- Arbitrary.arbitrary[BigDecimal]) yield JsNumber(bd)

	def cadena: Gen[String] = for {
		codePoint <- choose[Int](0, 0x10FFFF)
		list <- listOf(codePoint)
	} yield {
		val sb = new java.lang.StringBuilder(list.length * 2)
		list.foreach(sb.appendCodePoint)
		sb.toString
	}
	def jsString: Gen[JsString] = for {str <- cadena} yield JsString(str);

	def jsText: Gen[String] = for {js <- Arbitrary.arbitrary[JsValue]} yield js.prettyPrint

}
/** import the members of that object to use the implicit arbitrary[JSONType] */
object JsonGen extends JsonGen
