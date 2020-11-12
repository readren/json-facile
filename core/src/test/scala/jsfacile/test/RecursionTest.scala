package jsfacile.test

object RecursionTest {

	sealed trait Arbol[V]
	case class Rama[V](a: Arbol[V], b: Arbol[V]) extends Arbol[V]
	case class Hoja[V](v: V) extends Arbol[V]


	def main(args: Array[String]): Unit = {
		import jsfacile.api._

//		val ati = appenderOf[Tree[Int]]
//		println(ati);


		import scala.reflect.runtime.universe._

		TypeRef(
			SingleType(
				SingleType(
					SingleType(
						ThisType(_root_),
						jsfacile
					),
					jsfacile.test
				),
				jsfacile.test.RecursionTest
			),
			jsfacile.test.RecursionTest.Rama,
			List(
				TypeRef(
					ThisType(scala),
					scala.Int,
					List()
				)
			)
		)
	}

}
