package util

import org.scalacheck.Gen
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks}

class BinarySearchTest extends RefSpec with  ScalaCheckDrivenPropertyChecks {

	case class Elem(a: String, b: Int)

	val comparator: java.util.Comparator[Elem] =  (a, b) => a.b - b.b

	val genElem: Gen[Elem] =
		for {
			a <- Gen.alphaStr
			b <- Gen.choose(-99, 99)
		} yield Elem(a, b)


	val  genArrayAndIndex: Gen[(Array[Elem], Int)] =
		for {
			array <- Gen.containerOf[Array, Elem](genElem)
			if array.length > 0
			index <- Gen.choose(0, array.length-1)
		} yield {
			java.util.Arrays.sort(array, comparator)
			(array, index)
		}

	object `El binary search` {

		def `debe funcionar`(): Unit = {

			forAll(genArrayAndIndex) { case (array, index) =>

				val elemBuscado = array(index);
				index == BinarySearch.find(array) { e => e.b - elemBuscado.b}
			}
		}

	}
}
