package jsfacile.test

import jsfacile.api.{DiscriminatorDecider, FromJsonStringConvertible, MapFormatDecider, Record, ToJsonConvertible}
import jsfacile.test.SampleADT._
import jsfacile.write.PrefixInserter
import jsfacile.joint.{CoproductsOnly, ProductsOnly}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object AppenderMacrosTest {

	case class Simple[A](text: String, number: A)
	case class Nest[A](name: String, simple: Simple[A])
	case class Tree[N, A](height: Int, nests: List[N], mapa: Map[Simple[A], N])

	sealed trait Hueco {def id: Int}
	case class Vacio(id: Int) extends Hueco
	case class Lleno(id: Int, x2: Int) extends Hueco

	val simpleOriginal: Simple[Int] = Simple("hola", 7)
	val simpleJson = """{"text":"hola","number":7}""";
	val nestOriginal: Nest[Int] = Nest("chau", simpleOriginal)
	val nestJson = """{"name":"chau","simple":{"text":"hola","number":7}}"""
	val treeOriginal: Tree[Nest[Int], Int] = Tree(7, List(nestOriginal), Map(simpleOriginal -> nestOriginal))
}

class AppenderMacrosTest extends RefSpec with Matchers with ScalaCheckPropertyChecks {
	import AppenderMacrosTest._

	object `The appender should work ...` {

		def `for a single plain class`(): Unit = {
			assert(simpleJson == simpleOriginal.toJson)
		}

		def `with nested classes with type parameters`(): Unit = {
			assert(nestJson == nestOriginal.toJson)
		}

		def `with option and either`(): Unit = {
			assertResult("null")(None.toJson)
			assertResult("null")((None: Option[Simple[Int]]).toJson)
			assertResult(simpleJson)(Some(simpleOriginal).toJson)
			assertResult(simpleJson)((Some(simpleOriginal): Option[Simple[Int]]).toJson)

			assertResult(simpleJson)(Left(simpleOriginal).toJson)
			assertResult(simpleJson)((Left(simpleOriginal): Either[Simple[Int], Char]).toJson)
			assertResult(simpleJson)(Right(simpleOriginal).toJson)
			assertResult(simpleJson)((Right(simpleOriginal): Either[Char, Simple[Int]]).toJson)
		}

		def `with iterators and maps`(): Unit = {
			{
				val mapAsArray: String = treeOriginal.toJson;
				assert(mapAsArray == """{"height":7,"nests":[{"name":"chau","simple":{"text":"hola","number":7}}],"mapa":[[{"text":"hola","number":7},{"name":"chau","simple":{"text":"hola","number":7}}]]}""");
			}
			{
				implicit val mfd: MapFormatDecider[Simple[Int], Any, Any] = new MapFormatDecider[Simple[Int], Any, Any] {override val useObject: Boolean = true};
				val mapAsObjects: String = treeOriginal.toJson;
				assert(mapAsObjects == """{"height":7,"nests":[{"name":"chau","simple":{"text":"hola","number":7}}],"mapa":{"{\"text\":\"hola\",\"number\":7}":{"name":"chau","simple":{"text":"hola","number":7}}}}""")
			}
		}

		def `with abstract types`(): Unit = {
			val presentationDataJson = presentationDataOriginal.toJson
			val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
			assert(presentationDataParsed == Right(presentationDataOriginal))
		}

		def `with prefix inserter`(): Unit = {
			implicit val dd: DiscriminatorDecider[Simple[Int]] = new DiscriminatorDecider[Simple[Int]] {
				override def fieldName: String = "?"
				override def required: Boolean = true
			}
			implicit val pi: PrefixInserter[Simple[Int], ProductsOnly] = (record: Record, _: Simple[Int], isCoproduct: Boolean, symbol: String) => {
				assert(!isCoproduct && symbol == "Simple")
				record.append(s""" "insertado":"algo" """)
				true
			}
			val simpleJson = simpleOriginal.toJson
			assert(simpleJson == """{ "insertado":"algo" ,"text":"hola","number":7}""")

			implicit val huecoPi: PrefixInserter[Hueco, CoproductsOnly] = (record: Record, hueco: Hueco, isCoproduct: Boolean, symbol: String) => {
				assert(isCoproduct)
				hueco match {
					case Vacio(i) =>
						assert(symbol == "Vacio")
						record.append(s""""x2":${i * 2}""")
						true
					case _: Lleno =>
						assert(symbol == "Lleno")
						false
				}
			}

			val huecos = List[Hueco](Vacio(1), Lleno(2, 4), Vacio(3))
			val huecosJson = huecos.toJson
			val huecosParsed = huecosJson.fromJson[List[Hueco]]
			huecosParsed match {
				case Right(hs) =>
					assert(hs.forall {
						case Lleno(x, y) if y == 2 * x => true
						case _ => false
					})
				case Left(msg) =>
					fail(msg.toString)
			}
		}
	}

	object `Should not compile when ...` {

		def `appending a type constructed by a type constructor that has a subclass with a free type parameter`(): Unit = {
			"""
			  |object Probando {
			  |	sealed trait A[L] { def load: L };
			  |	case class A1[L](load: L) extends A[L];
			  |	case class A2[L, F](load: L, free: F) extends A[L]
			  |
			  |	def main(args: Array[String]): Unit = {
			  |		import jsfacile.api.write._
			  |  	val a: A[Int] = A2(3, "free")
			  |		val json = a.toJson
			  |	}
			  |}""".stripMargin shouldNot typeCheck
		}
	}
}
