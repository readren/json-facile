package jsfacile.macros

import scala.reflect.macros.whitebox

object whiteBoxCommon {

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		val implicits = ctx.enclosingImplicits.map { ic =>
			s"""|
				|	provider prefix: ${ic.pre},
				|	provider symbol: ${ic.sym.fullName},
				|	invoked type   : ${ic.pt},
				|	invoker code   : ${ic.tree}
				|""".stripMargin
		}.mkString("\nimplicits stack trace: [{", "},{", "}]")
		s"${new GenCommon(ctx).showEnclosingMacros}\n$implicits\n"
	}
}
