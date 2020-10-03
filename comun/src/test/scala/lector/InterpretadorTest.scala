package lector

import org.scalatest.refspec.RefSpec

class InterpretadorTest extends RefSpec {

	import Interpretador._

	object `Sea el contenido "Hola"` {

		def `aceptaElem('H') debe dar 'H' y avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult('H')(aceptaChar('H').interpretar(p))
			assert(p.pos == 1)
		}
		def `aceptaElem('P') debe dar nulo[Char] sin avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult(nulo[Elem])(aceptaChar('J').interpretar(p))
			assert(p.pos == 0)
		}

		def `aceptarStr("Hola") debe dar "Hola" y avanzar hasta el final`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult("Hola")(aceptaStr("Hola").interpretar(p))
			assert(p.pos == "Hola".length)
		}
		def `aceptarElem('Hello') debe dar nulo[String] sin avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult(nulo[String])(aceptaStr("Hello").interpretar(p))
			assert(p.pos == 0)
		}
	}
	object `Sea el contenido "El primero y el segundo"` {

		private val letra = aceptaElemSi(Character.isAlphabetic)
		private val digito = aceptaElemSi(Character.isDigit)
		private val espacio = aceptaElemSi(Character.isSpaceChar)

		def `la concatenacion funciona bien`(): Unit = {
			val p = new PunteroStr("El primero y el segundo");
			val i = ("El" ~ " ".rep) ~> "primero"
			assertResult("primero")(i.interpretar(p))
		}

		def `el | debe funcionar`(): Unit = {
			val p = new PunteroStr("El primero y el segundo");
			val i = "primero" | "segundo"
			val t = (letra.rep ~ espacio) ~> i ~ ((espacio ~ "y el" ~ espacio) ~> i)
			assertResult(new ~("primero", "segundo"))(t.interpretar(p))
		}

		def `el estricto debe funcionar`(): Unit = {
			val p = new PunteroStr("bla/1ble/2bli/3blo/otra");
			var testimonio = false;
			val i1 = '/' ~> digito.estricto
			val i2 = '#' ~> digito.map { x => testimonio = true; x }
			val t = (i1 | i2 | letra).rep
			t.interpretar(p)
			assert(p.pos == 18)
			assert(!testimonio)
		}
	}
}
