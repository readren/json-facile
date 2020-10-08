package lector


import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}

//noinspection TypeAnnotation
object LectorProductoTest extends DefaultJsonProtocol with LectoresJson {

	case class Simple(texto: String, numero: Long)
	case class Anidado(nombre: String, interno: Simple)

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


//noinspection TypeAnnotation
class LectorProductoTest extends RefSpec with Matchers { // with ScalaCheckDrivenPropertyChecks with JsonGen {
	import LectorProductoTest._
	import GuiaLectorProducto.materializeGuia

	val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

	object `Given some sample data type's instances...` {

		val simpleOriginal = Simple("hola", 5L)
		val simpleJson = simpleOriginal.toJson.prettyPrint
		val anidadoOriginal = Anidado("chau", Simple("hola", 5L))
		val anidadoJson = anidadoOriginal.toJson.prettyPrint

		val tableA = "table_A" -> Table(legsAmount = 4, description = "dinner room", enclosingShape = Box(List(Distance(1.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(750, DistanceUnit.Milimeter))));
		val shelfA = "shelf_A" -> Shelf(levelsAmount = 4, description = "for books", enclosingShape = Box(List(Distance(2.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(500, DistanceUnit.Milimeter))));
		val ballA = "ball_A" -> Ball(description = "soccer", enclosingShape = Sphere(radius = Distance(20, DistanceUnit.Milimeter)));
		val catalog = Map("table_A" -> BigDecimal(123.4), "shelf_A" -> BigDecimal(32.1))
		val inventory = Map("table_A" -> 4, "shelf_A" -> 3, "ball_A" -> 8)
		val presentationDataOriginal = PresentationData(catalog, inventory, Map(tableA, shelfA, ballA))
		val presentationDataJson = presentationDataOriginal.toJson.prettyPrint

		def `Implicit resolution of the interpreters should work`(): Unit = {
			//	import universe._
//			val rs = reify(implicitly[lector.GuiaLectorProducto[Simple]])

			val guiaSimple = implicitly[GuiaLectorProducto[Simple]];
			assert(guiaSimple != null && guiaSimple.infoCampos.nonEmpty && guiaSimple.infoCampos.forall(_._2.interpretador!=null))

			val lectorProducto = new LectorProducto[Simple]
			assert(lectorProducto != null && lectorProducto.interpretar(new PunteroStr(simpleJson)) == simpleOriginal)

			val iSimple = Interpretador.apply[Simple]
			assert(iSimple.isInstanceOf[LectorProducto[Simple]])
		}

		def `Json interpretation should work for a simple product`(): Unit = {
			val puntero = new PunteroStr(simpleJson)
			val iSimple = Interpretador.apply[Simple]
			val simpleInterpretado = iSimple.interpretar(puntero)
			assert(simpleInterpretado == simpleOriginal)
		}

		def `Json interpretation should work for ADTs with nested products`(): Unit = {
			val puntero = new PunteroStr(anidadoJson)
			val iAnidado = Interpretador.apply[Anidado]
			val anidadoInterpretado = iAnidado.interpretar(puntero)
			assert(anidadoInterpretado == anidadoOriginal)
		}

		def `Json interpretation should work for ADTs with both, products and coproducts`(): Unit = {
			val puntero = new PunteroStr(presentationDataJson)
			val iPresentationData = new LectorProducto[PresentationData]
			val presentationDataInterpretado = iPresentationData.interpretar(puntero)
			assert(presentationDataInterpretado == presentationDataOriginal)
		}
	}

}
