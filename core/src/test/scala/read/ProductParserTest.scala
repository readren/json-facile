package read


import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.{Outcome, Retries, Succeeded}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, enrichAny}
import util.SampleADT._

//noinspection TypeAnnotation
object ProductParserTest extends DefaultJsonProtocol {

	//////////////////////////////
	// Simple sample data types //

	case class Simple(text: String, number: Long)
	case class Nest(name: String, simple: Simple)
	case class Tree(height: Int, nests: List[Nest], mapa: Map[String, Simple])

	///////////////////////
	// Spray boilerplate //

	implicit val simpleFormat = jsonFormat2(Simple)
	implicit val anidadoFormat = jsonFormat2(Nest)
	implicit val treeFormat = jsonFormat3(Tree)


	class EnumJsonConverter[T <: scala.Enumeration](enu: T) extends RootJsonFormat[T#Value] {
		override def write(obj: T#Value): JsValue = JsString(obj.toString)

		override def read(json: JsValue): T#Value = {
			json match {
				case JsString(txt) => enu.withName(txt)
				case somethingElse => throw DeserializationException(s"Expected a value from enum $enu instead of $somethingElse")
			}
		}
	}

	implicit val distanceUnitFormat = new EnumJsonConverter(DistanceUnit)
	implicit val distanceFormat = jsonFormat2(Distance)
	implicit val boxFormat = jsonFormat1(Box)
	implicit val sphereFormat = jsonFormat1(Sphere);
	implicit val shapeFormat = new RootJsonFormat[Shape] {
		override def read(json: JsValue): Shape = ???
		override def write(obj: Shape): JsValue = obj match {
			case b: Box => boxFormat.write(b)
			case s: Sphere => sphereFormat.write(s);
		}
	}
	implicit val tableFormat = jsonFormat3(Table)
	implicit val shelfFormat = jsonFormat3(Shelf)
	implicit val ballFormat = jsonFormat2(Ball)
	implicit val thingFormat = new RootJsonFormat[Thing] {
		override def read(json: JsValue): Thing = ???
		override def write(obj: Thing): JsValue = obj match {
			case t: Table => tableFormat.write(t)
			case s: Shelf => shelfFormat.write(s);
			case b: Ball => ballFormat.write(b);
		}
	}
	implicit val presentationDataFormat = jsonFormat3(PresentationData)

	////////////////////////
	// Simple sample data //

	val simpleOriginal = Simple("hola", 5L)
	val simpleJson = simpleOriginal.toJson.prettyPrint
	val nestOriginal = Nest("chau", Simple("hola", 5L))
	val nestJson = nestOriginal.toJson.prettyPrint
	val treeOriginal = Tree(7, List(nestOriginal), Map("clave" -> simpleOriginal))
	val treeJson = treeOriginal.toJson.prettyPrint;

	val presentationDataJson = presentationDataOriginal.toJson.prettyPrint


}


//noinspection TypeAnnotation
class ProductParserTest extends RefSpec with Matchers with Retries { // with ScalaCheckDrivenPropertyChecks with JsonGen {
	import ProductParserTest._
	import read._

//	private val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

	object `Given sample ADTs...` {

		def `Implicit resolution of the interpreters should work`(): Unit = {
			//	import universe._
//			val rs = reify(implicitly[lector.GuiaLectorProducto[Simple]])

			val simpleHelper = ProductParserHelper.materializeHelper[Simple];
			assert(simpleHelper != null && simpleHelper.fieldsInfo.nonEmpty && simpleHelper.fieldsInfo.forall(_._2.valueParser != null))

			val productParser = new ProductParser[Simple](simpleHelper)
			assert(productParser != null && productParser.parse(new CursorStr(simpleJson)) == simpleOriginal)

			val simpleParser = Parser.apply[Simple]
			assert(simpleParser.isInstanceOf[ProductParser[Simple]])
		}

		def `Json interpretation should work for a simple product`(): Unit = {
			val simpleParsed = simpleJson.fromJson[Simple]
			assertResult(simpleOriginal)(simpleParsed)
		}

		def `Json interpretation should work for nested products`(): Unit = {
			val nestParsed = nestJson.fromJson[Nest]
			assertResult(nestOriginal)(nestParsed)
		}

		def `Json interpretation should work for products with iterables`(): Unit = {
			val treeParsed = treeJson.fromJson[Tree]
			assertResult(treeOriginal)(treeParsed)
		}

		def `Json interpretation should work for simple ADTs with a coproduct`(): Unit = {
			val tableAParsed = tableA._2.toJson.prettyPrint.fromJson[Table]
			assertResult(tableA._2)(tableAParsed)

			val ballParsed = ballA._2.toJson.prettyPrint.fromJson[Ball]
			assertResult(ballA._2)(ballParsed)

			val shelfAParsed = shelfA._2.toJson.prettyPrint.fromJson[Shelf]
			assertResult(shelfA._2)(shelfAParsed)
		}

		def `Json interpretation should work for complex ADTs`(): Unit = {
			val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
			assertResult(presentationDataOriginal)(presentationDataParsed)
		}
	}


	override def withFixture(test: NoArgTest) = {
		withFixture(test, 9)
	}

	def withFixture(test: NoArgTest, count: Int): Outcome = {
		val outcome = super.withFixture(test)
		outcome match {
			case Succeeded if count > 0 =>
				withFixture(test, count - 1)

			case other => other
		}
	}
}
