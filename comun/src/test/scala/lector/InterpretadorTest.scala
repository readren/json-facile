package lector

import org.scalatest.refspec.RefSpec

class InterpretadorTest extends RefSpec {

	import Interpretador._

	object `Sea el contenido "Hola"` {

		def `aceptaElem('H') debe dar 'H' y avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult('H')(aceptaChar('H').interpretar(p))
			assert(p.ok && !p.fallado && !p.enFin)
			assert(p.pos == 1)
		}
		def `aceptaElem('P') debe dar nulo[Char] sin avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			aceptaChar('J').interpretar(p);
			assert(!p.ok && !p.fallado)
			assert(p.pos == 0)
		}

		def `aceptarStr("Hola") debe dar "Hola" y avanzar hasta el final`(): Unit = {
			val p = new PunteroStr("Hola");
			assertResult("Hola")(aceptaStr("Hola").interpretar(p))
			assert(p.ok && !p.fallado && p.enFin)
		}
		def `aceptarElem('Hello') debe fracasar sin avanzar`(): Unit = {
			val p = new PunteroStr("Hola");
			aceptaStr("Hello").interpretar(p)
			assert(!p.ok && !p.fallado)
			assert(p.pos == 0)
		}
	}
	object `Sea el contenido "El primero y el segundo"` {

		private val letra = aceptaElemSi(Character.isAlphabetic)
		private val digito = aceptaElemSi(Character.isDigit)
		private val espacio = aceptaElemSi(Character.isSpaceChar)

		def `la concatenacion funciona bien`(): Unit = {
			val p = new PunteroStr("El primero y el segundo");
			val i = ("El" ~ ' '.rep) ~> "primero" ~ (" y el " ~> (aceptaElemSi(Character.isAlphabetic).rep1 ^^ (_.map(_.toChar).mkString)))
			assertResult(new ~("primero", "segundo"))(i.interpretar(p))
			assert(p.ok && !p.fallado && p.enFin)
		}

		def `el | debe funcionar`(): Unit = {
			val p = new PunteroStr("El primero y el segundo");
			val i = "primero" | "segundo"
			val t = (letra.rep ~ espacio) ~> i ~ ((espacio ~ "y el" ~ espacio) ~> i)
			assertResult(new ~("primero", "segundo"))(t.interpretar(p))
			assert(p.ok && p.enFin && !p.fallado && !p.fracasado)
		}

		def `el orFail debe funcionar`(): Unit = {
			val p = new PunteroStr("bla/1ble/2bli/3blo/otra");
			val escape = '/' ~> digito.orFail
			val t = (escape | letra).rep ^^ {_.map(_.toChar).mkString}
			t.interpretar(p)
			assert(!p.ok && p.fallado && p.pos == 19)
		}
		def `el recoverWith debe funcionar`(): Unit = {
			val p = new PunteroStr("bla/1ble/bli/3blo");
			val escape = '/' ~> digito.orFail;
			val t = (letra | escape).rep1.recover(List('-'.toInt)).rep ^^ { x => x.flatten.map(_.toChar).mkString }
			assertResult("-bli3blo")(t.interpretar(p));
			assert(p.ok && p.enFin && !p.fallado);
		}
	}
}
