package jsfacile.read

import org.scalatest.refspec.RefSpec

class ParserTest extends RefSpec {

	import Parser._

	object `Sea el contenido "Hola"` {

		def `acceptChar('H') should hit, give 'H' and advance`(): Unit = {
			val p = new CursorStr("Hola");
			assertResult('H')(acceptChar('H').parse(p))
			assert(p.ok && !p.failed && !p.atEnd)
			assert(p.pos == 1)
		}
		def `acceptChar('P') should miss and stay`(): Unit = {
			val p = new CursorStr("Hola");
			acceptChar('J').parse(p);
			assert(!p.ok && !p.failed)
			assert(p.pos == 0)
		}

		def `acceptStr("Hola") should hit, give "Hola", and avance until end`(): Unit = {
			val p = new CursorStr("Hola");
			assertResult("Hola")(acceptStr("Hola").parse(p))
			assert(p.ok && !p.failed && p.atEnd)
		}
		def `acceptStr('Hello') should miss and stay`(): Unit = {
			val p = new CursorStr("Hola");
			acceptStr("Hello").parse(p)
			assert(!p.ok && !p.failed)
			assert(p.pos == 0)
		}
	}
	object `Sea el contenido "El primero y el segundo"` {

		private val alpha = acceptElemIf(Character.isAlphabetic)
		private val digit = acceptElemIf(Character.isDigit)
		private val space = acceptElemIf(Character.isSpaceChar)

		def `pursue should work`(): Unit = {
			val p = new CursorStr("El primero y el segundo");
			val i = ("El" ~ ' '.rep) ~> "primero" ~ (" y el " ~> (acceptElemIf(Character.isAlphabetic).rep1 ^^ (_.map(_.toChar).mkString)))
			assertResult(new ~("primero", "segundo"))(i.parse(p))
			assert(p.ok && !p.failed && p.atEnd)
		}

		def `orElse should work`(): Unit = {
			val p = new CursorStr("El primero y el segundo");
			val i = "primero" | "segundo"
			val t = (alpha.rep ~ space) ~> i ~ ((space ~ "y el" ~ space) ~> i)
			assertResult(new ~("primero", "segundo"))(t.parse(p))
			assert(p.ok && p.atEnd && !p.failed && !p.missed)
		}

		def `orFail should work`(): Unit = {
			val p = new CursorStr("bla/1ble/2bli/3blo/otra");
			val escape = '/' ~> digit.orFail("digit expected after '/` ")
			val t = (escape | alpha).rep ^^ {_.map(_.toChar).mkString}
			t.parse(p)
			assert(!p.ok && p.failed && p.pos == 19)
		}
		def `recover shouldWork`(): Unit = {
			val p = new CursorStr("bla/1ble/bli/3blo");
			val escape = '/' ~> digit.orFail("digit expected after '/'");
			val t = (alpha | escape).rep1.recover(List('-'.toInt)).rep ^^ { x => x.flatten.map(_.toChar).mkString }
			assertResult("-bli3blo")(t.parse(p));
			assert(p.ok && p.atEnd && !p.failed);
		}
	}
}
