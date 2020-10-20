package write

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.JsonGen
import write.ProductAppenderTest._

object ProductAppenderTest {

	case class Simple[A](text: String, number: A)
	case class Nest[A](name: String, simple: Simple[A])
	case class Tree[N, A](height: Int, nests: List[N], mapa: Map[Simple[A], N])

	val simpleOriginal = Simple("hola", 7)
	val nestOriginal = Nest("chau", simpleOriginal)
	val treeOriginal = Tree(7, List(nestOriginal), Map(simpleOriginal -> nestOriginal))

}

class ProductAppenderTest extends RefSpec with Matchers with ScalaCheckPropertyChecks with JsonGen {

	import ProductAppender.materialize
//	import IterableAppender.apply
	import MapAppender.apply


	object `The appender should work for ...` {

		def `a single plain class`(): Unit = {
			val simpleJson = simpleOriginal.toJson
			assert(simpleJson == """{"text":"hola","number":7}""")
		}

		def `nested classes`(): Unit = {
			val nestJson = nestOriginal.toJson
			assert(nestJson == """{"name":"chau","simple":{"text":"hola","number":7}}""")
		}

		def `iterators and maps`(): Unit = {
			val treeJson = treeOriginal.toJson
			assert(treeJson == """{"height":7,"nests":[{"name":"chau","simple":{"text":"hola","number":7}}],"mapa":{"{\"text\":\"hola\",\"number\":7}":{"name":"chau","simple":{"text":"hola","number":7}}}}""")
		}

		def `nested classes with type parameters`(): Unit = {

		}
	}


}