package jsfacile.macros

import scala.reflect.macros.whitebox

object whiteBoxCommon {

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		val implicits = getEnclosingImplicits(ctx);

		s"${new GenCommon(ctx).showEnclosingMacros}\n$implicits\n"
	}

	def getEnclosingImplicits(ctx: whitebox.Context): String = {
		ctx.openImplicits.map { ic =>
			s"""|
				|	provider prefix: ${ic.pre},
				|	provider symbol: ${ic.sym.fullName},
				|	invoked type   : ${ic.pt},
				|	invoker code   : ${ic.tree}
				|""".stripMargin
		}.mkString("\nimplicits stack trace: [{", "},{", "}]")
	}
}
