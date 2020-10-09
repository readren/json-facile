package read

import org.scalatest.refspec.RefSpec

class ParserTest extends RefSpec {

	import Parser._

	object `Sea el contenido "Hola"` {

		def `aceptaElem('H') debe dar 'H' y avanzar`(): Unit = {
			val p = new CursorStr("Hola");
			assertResult('H')(acceptChar('H').parse(p))
			assert(p.ok && !p.failed && !p.atEnd)
			assert(p.pos == 1)
		}
		def `aceptaElem('P') debe dar nulo[Char] sin avanzar`(): Unit = {
			val p = new CursorStr("Hola");
			acceptChar('J').parse(p);
			assert(!p.ok && !p.failed)
			assert(p.pos == 0)
		}

		def `aceptarStr("Hola") debe dar "Hola" y avanzar hasta el final`(): Unit = {
			val p = new CursorStr("Hola");
			assertResult("Hola")(acceptStr("Hola").parse(p))
			assert(p.ok && !p.failed && p.atEnd)
		}
		def `aceptarElem('Hello') debe fracasar sin avanzar`(): Unit = {
			val p = new CursorStr("Hola");
			acceptStr("Hello").parse(p)
			assert(!p.ok && !p.failed)
			assert(p.pos == 0)
		}
	}
	object `Sea el contenido "El primero y el segundo"` {

		private val letra = acceptElemIf(Character.isAlphabetic)
		private val digito = acceptElemIf(Character.isDigit)
		private val espacio = acceptElemIf(Character.isSpaceChar)

		def `la concatenacion funciona bien`(): Unit = {
			val p = new CursorStr("El primero y el segundo");
			val i = ("El" ~ ' '.rep) ~> "primero" ~ (" y el " ~> (acceptElemIf(Character.isAlphabetic).rep1 ^^ (_.map(_.toChar).mkString)))
			assertResult(new ~("primero", "segundo"))(i.parse(p))
			assert(p.ok && !p.failed && p.atEnd)
		}

		def `el | debe funcionar`(): Unit = {
			val p = new CursorStr("El primero y el segundo");
			val i = "primero" | "segundo"
			val t = (letra.rep ~ espacio) ~> i ~ ((espacio ~ "y el" ~ espacio) ~> i)
			assertResult(new ~("primero", "segundo"))(t.parse(p))
			assert(p.ok && p.atEnd && !p.failed && !p.missed)
		}

		def `el orFail debe funcionar`(): Unit = {
			val p = new CursorStr("bla/1ble/2bli/3blo/otra");
			val escape = '/' ~> digito.orFail
			val t = (escape | letra).rep ^^ {_.map(_.toChar).mkString}
			t.parse(p)
			assert(!p.ok && p.failed && p.pos == 19)
		}
		def `el recoverWith debe funcionar`(): Unit = {
			val p = new CursorStr("bla/1ble/bli/3blo");
			val escape = '/' ~> digito.orFail;
			val t = (letra | escape).rep1.recover(List('-'.toInt)).rep ^^ { x => x.flatten.map(_.toChar).mkString }
			assertResult("-bli3blo")(t.parse(p));
			assert(p.ok && p.atEnd && !p.failed);
		}
	}
}
