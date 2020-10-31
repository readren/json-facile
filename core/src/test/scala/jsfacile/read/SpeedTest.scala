package jsfacile.read

import jsfacile.api.write._
import jsfacile.api.read._
import jsfacile.util.SampleADT._
import jsfacile.read.ProductParserTest._
import jsfacile.read.Parser;

object SpeedTest {
	class ReseteableCursor(content: String) extends jsfacile.read.CursorStr(content) {
		def restart: Unit = this.cursorPos = 0;
	}

	def main(args: Array[String]) {

		val ss = SyntaxParsers.skipSpaces;
		val c = new CursorStr("    ")
		var x = ss.parse(c)

		val presentationDataJson = presentationDataOriginal.toJson
		val ppd = Parser[PresentationData]

		val cursor = new ReseteableCursor(presentationDataJson)
		ppd.parse(cursor);
		cursor.restart;

		val start = java.lang.System.nanoTime();
		for (i <- 0 to 100000) {
			cursor.restart;
			ppd.parse(cursor);
		}
		println((java.lang.System.nanoTime() - start) / 1000000000f);
	}
}
// -Ymacro-debug-lite