package jsfacile.macros

import scala.reflect.macros.whitebox

import jsfacile.read.Parser
import jsfacile.write.Appender


object NothingMacros {

	def materializeNothingParserImpl(ctx: whitebox.Context): ctx.Expr[Parser[Nothing]] = {
		import ctx.universe._

		ctx.info(ctx.enclosingPosition, s"Note that a `Parser[Nothing]` is involved here. If you get a compiler error at this line, this could be the cause. Probably the `Nothing` type was inferred by the compiler because a type parameter is not specified.\n${showOpenImplicitsAndMacros(ctx)}", force = true);

		val body = q"""
import _root_.jsfacile.read.{Parser, Cursor};
import _root_.jsfacile.macros.Probe;

new Parser[Nothing] {
	override def parser(cursor: Cursor): Nothing = throw new RuntimeException("It's not possible to to parse `Nothing`");
}"""
		ctx.Expr[Parser[Nothing]](body)
	}


	def materializeNothingAppenderImpl(ctx: whitebox.Context): ctx.Expr[Appender[Nothing]] = {
		import ctx.universe._

		ctx.info(ctx.enclosingPosition, s"Note that an `Appender[Nothing]` is involved here. If you get a compiler error at this line, this could be the cause. Probably the `Nothing` type was inferred by the compiler because a type parameter is not specified.:\n${showOpenImplicitsAndMacros(ctx)}", force = true);

		val body = q"""
import _root_.jsfacile.write.{Appender, Record};
import _root_.jsfacile.macros.Probe;

new Appender[Nothing] {
	override def append(rec: Record, nothing: Nothing): Record = rec;
}"""
		ctx.Expr[Appender[Nothing]](body)
	}

}
