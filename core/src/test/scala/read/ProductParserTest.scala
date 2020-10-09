package read


import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}

//noinspection TypeAnnotation
object ProductParserTest extends DefaultJsonProtocol with JsonParsers {

	case class Simple(text: String, number: Long)
	case class Anidado(name: String, simple: Simple)

	object DistanceUnit extends Enumeration {
		type DistanceUnit = Value
		val Meter, Milimeter = Value;
	}

	case class Distance(value: Double, unit: DistanceUnit.Value)

	sealed trait Shape
	case class Box(axis: List[Distance]) extends Shape
	case class Sphere(radius: Distance) extends Shape

	trait Thing {
		def enclosingShape: Shape
		def description: String
	}
	case class Table(enclosingShape: Shape, legsAmount: Int, description: String) extends Thing
	case class Shelf(enclosingShape: Shape, levelsAmount: Int, description: String) extends Thing
	case class Ball(enclosingShape: Shape, description: String) extends Thing

	type Price = BigDecimal
	type ThingId = String
	type Catalog = Map[ThingId, Price]
	type Inventory = Map[ThingId, Int]
	case class PresentationData(catalog: Catalog, inventory: Inventory, things: Map[ThingId, Thing])

	// ---- //
	private implicit val simpleFormat = jsonFormat2(Simple)
	private implicit val anidadoFormat = jsonFormat2(Anidado)

	class EnumJsonConverter[T <: scala.Enumeration](enu: T) extends RootJsonFormat[T#Value] {
		override def write(obj: T#Value): JsValue = JsString(obj.toString)

		override def read(json: JsValue): T#Value = {
			json match {
				case JsString(txt) => enu.withName(txt)
				case somethingElse => throw DeserializationException(s"Expected a value from enum $enu instead of $somethingElse")
			}
		}
	}

	private implicit val distanceUnitFormat = new EnumJsonConverter(DistanceUnit)
	private implicit val distanceFormat = jsonFormat2(Distance)
	private implicit val boxFormat = jsonFormat1(Box)
	private implicit val sphereFormat = jsonFormat1(Sphere);
	private implicit val shapeFormat = new RootJsonFormat[Shape] {
		override def read(json: JsValue): Shape = ???
		override def write(obj: Shape): JsValue = obj match {
			case b: Box => boxFormat.write(b)
			case s: Sphere => sphereFormat.write(s);
		}
	}
	private implicit val tableFormat = jsonFormat3(Table)
	private implicit val shelfFormat = jsonFormat3(Shelf)
	private implicit val ballFormat = jsonFormat2(Ball)
	private implicit val thingFormat = new RootJsonFormat[Thing] {
		override def read(json: JsValue): Thing = ???
		override def write(obj: Thing): JsValue = obj match {
			case t: Table => tableFormat.write(t)
			case s: Shelf => shelfFormat.write(s);
			case b: Ball => ballFormat.write(b);
		}
	}
	private implicit val presentationDataFormat = jsonFormat3(PresentationData)

}



class ProductParserTest extends RefSpec with Matchers { // with ScalaCheckDrivenPropertyChecks with JsonGen {
	import ProductParserTest._
	import ProductParserHelper.materializeHelper

	private val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

	object `Given some sample data type's instances...` {

		private val simpleOriginal = Simple("hola", 5L)
		private val simpleJson = simpleOriginal.toJson.prettyPrint
		private val anidadoOriginal = Anidado("chau", Simple("hola", 5L))
		private val anidadoJson = anidadoOriginal.toJson.prettyPrint
		private val tableA = "table_A" -> Table(legsAmount = 4, description = "dinner room", enclosingShape = Box(List(Distance(1.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(750, DistanceUnit.Milimeter))));
		private val shelfA = "shelf_A" -> Shelf(levelsAmount = 4, description = "for books", enclosingShape = Box(List(Distance(2.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(500, DistanceUnit.Milimeter))));
		private val ballA = "ball_A" -> Ball(description = "soccer", enclosingShape = Sphere(radius = Distance(20, DistanceUnit.Milimeter)));
		private val catalog = Map("table_A" -> BigDecimal(123.4), "shelf_A" -> BigDecimal(32.1))
		private val inventory = Map("table_A" -> 4, "shelf_A" -> 3, "ball_A" -> 8)
		private val presentationDataOriginal = PresentationData(catalog, inventory, Map(tableA, shelfA, ballA))
		private val presentationDataJson = presentationDataOriginal.toJson.prettyPrint

		def `Implicit resolution of the interpreters should work`(): Unit = {
			//	import universe._
//			val rs = reify(implicitly[lector.GuiaLectorProducto[Simple]])

			val simpleHelper = implicitly[ProductParserHelper[Simple]];
			assert(simpleHelper != null && simpleHelper.fieldsInfo.nonEmpty && simpleHelper.fieldsInfo.forall(_._2.valueParser != null))

			val productParser = new ProductParser[Simple]
			assert(productParser != null && productParser.parse(new CursorStr(simpleJson)) == simpleOriginal)

			val simpleParser = Parser.apply[Simple]
			assert(simpleParser.isInstanceOf[ProductParser[Simple]])
		}

		def `Json interpretation should work for a simple product`(): Unit = {
			val cursor = new CursorStr(simpleJson)
			val simpleParser = Parser.apply[Simple]
			val simpleParsed = simpleParser.parse(cursor)
			assert(simpleParsed == simpleOriginal)
		}

		def `Json interpretation should work for ADTs with nested products`(): Unit = {
			val cursor = new CursorStr(anidadoJson)
			val anidadoParser = Parser.apply[Anidado]
			val anidadoParsed = anidadoParser.parse(cursor)
			assert(anidadoParsed == anidadoOriginal)
		}

		def `Json interpretation should work for ADTs with both, products and coproducts`(): Unit = {
			val cursor = new CursorStr(presentationDataJson)
			val presentationDataParser = new ProductParser[PresentationData]
			val presentationDataParsed = presentationDataParser.parse(cursor)
			assert(presentationDataParsed == presentationDataOriginal)
		}
	}

}
