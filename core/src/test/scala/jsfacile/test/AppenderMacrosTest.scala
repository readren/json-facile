package jsfacile.test

import SampleADT._

import jsfacile.macros.Tools
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object AppenderMacrosTest {

	case class Simple[A](text: String, number: A)
	case class Nest[A](name: String, simple: Simple[A])
	case class Tree[N, A](height: Int, nests: List[N], mapa: Map[Simple[A], N])

	val simpleOriginal: Simple[Int] = Simple("hola", 7)
	val nestOriginal: Nest[Int] = Nest("chau", simpleOriginal)
	val treeOriginal: Tree[Nest[Int], Int] = Tree(7, List(nestOriginal), Map(simpleOriginal -> nestOriginal))


}

class AppenderMacrosTest extends RefSpec with Matchers with ScalaCheckPropertyChecks {
	import AppenderMacrosTest._
	import jsfacile.api._

	object `The appender should work ...` {

		def `for a single plain class`(): Unit = {
			val simpleJson = simpleOriginal.toJson
			assert(simpleJson == """{"text":"hola","number":7}""")
		}

		def `with nested classes with type parameters`(): Unit = {
			val nestJson = nestOriginal.toJson
			assert(nestJson == """{"name":"chau","simple":{"text":"hola","number":7}}""")
		}

		def `with iterators and maps`(): Unit = {
			{
				val mapAsArray: String = treeOriginal.toJson;
				assert(mapAsArray == """{"height":7,"nests":[{"name":"chau","simple":{"text":"hola","number":7}}],"mapa":[[{"text":"hola","number":7},{"name":"chau","simple":{"text":"hola","number":7}}]]}""");
			}
			{
				implicit val mfd = new MapFormatDecider[Simple[Int], Any, Any] {override val useObject: Boolean = true};
				Tools.clearAppenderBufferOf[Tree[Nest[Int], Int]];

				val mapAsObjects: String = treeOriginal.toJson;
				assert(mapAsObjects == """{"height":7,"nests":[{"name":"chau","simple":{"text":"hola","number":7}}],"mapa":{"{\"text\":\"hola\",\"number\":7}":{"name":"chau","simple":{"text":"hola","number":7}}}}""")
			}
		}

		def `with abstract types`(): Unit = {
			val presentationDataJson = presentationDataOriginal.toJson
			val presentationDataParsed = presentationDataJson.fromJson[PresentationData]
			assert(presentationDataParsed == Right(presentationDataOriginal))
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
