package jsfacile.test

import jsfacile.macros.ProductParserHelper
import jsfacile.read.{Parser, ProductParser}
import jsfacile.util.{::, Base}
import SampleADT._
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatest.{Outcome, Retries, Succeeded}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat}

//noinspection TypeAnnotation
object ParserMacrosTest extends DefaultJsonProtocol {

	//////////////////////////////
	// Simple sample data types //

	case class Simple(text: String, number: Long);
	case class Nest(name: String, simple: Simple);
	case class Tree(height: Int, nests: List[Nest], mapa: Map[String, Simple]);

	//////////////////////////////////
	// Recursive sample data types //

	sealed trait Foo[T];
	case class FooNext[T](next: Foo[T]) extends Foo[T];
	case class FooBase[T](v: T) extends Foo[T];

	sealed trait Arbol[V];
	case class Rama[V](a: Arbol[V], b: Arbol[V]) extends Arbol[V];
	case class Hoja[V](v: V) extends Arbol[V];

	///////////////////////
	// Spray boilerplate //

	implicit val simpleFormat = jsonFormat2(Simple);
	implicit val anidadoFormat = jsonFormat2(Nest);
	implicit val treeFormat = jsonFormat3(Tree);


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
		override def read(json: JsValue): Shape = {
			json.asJsObject.fields("type") match {
				case JsString("Box") => boxFormat.read(json)
				case JsString("Sphere") => sphereFormat.read(json)
			}
		}
		override def write(obj: Shape): JsValue = obj match {
			case b: Box => JsObject(boxFormat.write(b).asJsObject.fields + ("type" -> JsString("Box")))
			case s: Sphere => JsObject(sphereFormat.write(s).asJsObject.fields + ("type" -> JsString("Sphere")))
		}
	}
	implicit val tableFormat = jsonFormat3(Table)
	implicit val shelfFormat = jsonFormat3(Shelf)
	implicit val ballFormat = jsonFormat2(Ball)
	implicit val thingFormat = new RootJsonFormat[Thing] {
		override def read(json: JsValue): Thing = {
			json.asJsObject.fields("type") match {
				case JsString("Table") => tableFormat.read(json)
				case JsString("Shelf") => shelfFormat.read(json)
				case JsString("Ball") => ballFormat.read(json)
			}
		}
		override def write(obj: Thing): JsValue = obj match {
			case t: Table => new JsObject(tableFormat.write(t).asJsObject.fields + ("type" -> JsString("Table")))
			case s: Shelf => new JsObject(shelfFormat.write(s).asJsObject.fields + ("type" -> JsString("Shelf")))
			case b: Ball => new JsObject(ballFormat.write(b).asJsObject.fields + ("type" -> JsString("Ball")));
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

}


//noinspection TypeAnnotation
class ParserMacrosTest extends RefSpec with Matchers with Retries { // with ScalaCheckDrivenPropertyChecks with JsonGen {
	import ParserMacrosTest._

	object `Given sample ADTs...` {

		def `Implicit resolution of the interpreters should work`(): Unit = {
			import jsfacile.api.CursorStr

			val simpleHelper = ProductParserHelper.materialize[Simple];
			assert(simpleHelper != null && simpleHelper.fieldsInfo.nonEmpty && simpleHelper.fieldsInfo.forall(_.valueParser != null))

			val productParser = new ProductParser[Simple](simpleHelper)
			assert(productParser != null && productParser.parse(new CursorStr(simpleJson)) == simpleOriginal)

			val simpleParser = Parser.apply[Simple]
			assert(simpleParser.isInstanceOf[ProductParser[Simple]])
		}
	}

	object `Json interpretation should work ...` {
		import jsfacile.api.{FromJsonConvertable, ToJsonConvertable, CursorStr}

		def `for a simple product`(): Unit = {
			val simpleParsed = simpleJson.fromJson[Simple]
			assertResult(Right(simpleOriginal))(simpleParsed)
		}

		def `for nested products`(): Unit = {
			val nestParsed = nestJson.fromJson[Nest]
			assertResult(Right(nestOriginal))(nestParsed)
		}

		def `for products with iterables`(): Unit = {
			val treeParsed = treeJson.fromJson[Tree]
			assertResult(Right(treeOriginal))(treeParsed)
		}

		def `for simple ADTs with a coproduct`(): Unit = {
			import spray.json.enrichAny;

			val tableAParsed = enrichAny(tableA._2).toJson.prettyPrint.fromJson[Table]
			assertResult(Right(tableA._2))(tableAParsed)

			val ballParsed = enrichAny(ballA._2).toJson.prettyPrint.fromJson[Ball]
			assertResult(Right(ballA._2))(ballParsed)

			val shelfAParsed = enrichAny(shelfA._2).toJson.prettyPrint.fromJson[Shelf]
			assertResult(Right(shelfA._2))(shelfAParsed)
		}

		def `for complex ADTs`(): Unit = {
			import jsfacile.api._

			val presentationDataJson = presentationDataOriginal.toJson
			val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
			assertResult(Right(presentationDataOriginal))(presentationDataParsed)
		}

		def `with HLists`(): Unit = {
			import jsfacile.api._
			val pilaOriginal = "top" :: 3 :: true :: Base
			val pilaJson = pilaOriginal.toJson
			val pilaParsed = pilaJson.fromJson[String :: Int :: Boolean :: Base.type]
			assertResult(Right(pilaOriginal))(pilaParsed)
		}

		def `with recursive data types`(): Unit = {
			import jsfacile.api._

			val fooBase = FooBase(7);
			val fooNext = FooNext(fooBase)
			val fooJson = fooNext.toJson
			val fooParsed = fooJson.fromJson[Foo[Int]]
			assertResult(Right(fooNext))(fooParsed)

			val arbol = Rama(Rama(Hoja(1), Hoja(2)), Hoja(3))
			val json = arbol.toJson
			val arbolParsed = json.fromJson[Arbol[Int]]
			assertResult(Right(arbol))(arbolParsed)
		}


		sealed trait A[L] {def load: L};
		case class A1[L](load: L) extends A[L]
		case object A2 extends A[Int] {def load = 3}

		sealed trait B[L] extends A[L]
		case class B1[L](load: L, extra: Long) extends B[L]
		case object B2 extends B[Float] {def load = 1.2f}

		sealed trait C[L] extends B[L]
		case class C1[L](load: L) extends C[L]
		case object C2 extends C[String] {def load = "fixed"}


		def `when traits and/or abstract classes are nested directly (no intermediate product)`(): Unit = {
			import jsfacile.api._

			val set: Set[A[String]] = Set(A1("primero"), B1("dudo", 7), C1("también"), C2)
			val json = ToJsonConvertable(set).toJson
			val parsed = json.fromJson[Set[A[String]]]
			assertResult(Right(set.toList))(parsed.map(_.toList))

			assert("{}".fromJson[A[Int]] == Right(A2))
		}
	}

	object `Should not compile when ...` {

		sealed trait A[L] { def load: L };
		case class A1[L](load: L) extends A[L];
		case class A2[L, F](load: L, free: F) extends A[L]

		def `parsing a type constructed by a type constructor that has a subclass with a free type parameter`(): Unit = {
			import jsfacile.api._
			"""val aParsed = "?".fromJson[A[Double]]""" shouldNot typeCheck
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
