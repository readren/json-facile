package jsfacile.macros

import scala.language.implicitConversions
import scala.reflect.macros.whitebox

import jsfacile.read.Parser
import jsfacile.write.Appender

/** Useful to debug implicit resolution problems.
 * Apply this type constructor to a type that is involved in an implicit search to emmit log messages with the implicit and macro stack traces. */
case class Probe[T](value: T)

object Probe {

	implicit def unlifted[T](probe: Probe[T]): T = probe.value;

	implicit def lifted[T](t: T): Probe[T] = new Probe(t);

	implicit def materialize[T]: Probe[T] = macro materializeImpl[T]

	def materializeImpl[T : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Probe[T]] = {
		import ctx.universe._

		val tpe: Type = ctx.weakTypeTag[T].tpe.dealias;

		ctx.info(ctx.enclosingPosition, s"Providing implicit Probe[$tpe]:\n${whiteBoxCommon.showOpenImplicitsAndMacros(ctx)}", force = false);

		ctx.Expr[Probe[T]](q"_root_.jsfacile.macros.Probe[$tpe](_root_.scala.Predef.implicitly[$tpe])")
	}


	implicit def materializeAppender[T]: Appender[Probe[T]] = macro materializeAppenderImpl[T]

	def materializeAppenderImpl[T : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[Probe[T]]] = {
		import ctx.universe._

		val tpe: Type = ctx.weakTypeTag[T].tpe.dealias;

		ctx.info(ctx.enclosingPosition, s"Appending Probe[$tpe]:\n${whiteBoxCommon.showOpenImplicitsAndMacros(ctx)}", force = false);

		val body = q"""
import _root_.jsfacile.write.{Appender, Record};
import _root_.jsfacile.macros.Probe;

new Appender[Probe[$tpe]] {
	override def append(rec: Record, probe: Probe[$tpe]): Record = {
		rec.appendSummoned[$tpe](probe.value)
	}
}"""

		ctx.Expr[Appender[Probe[T]]](body)
	}


	implicit def materializeParser[T](implicit pt: Parser[T]): Parser[Probe[T]] = macro materializeParserImpl[T]

	def materializeParserImpl[T : ctx.WeakTypeTag](ctx: whitebox.Context)(pt: ctx.Expr[Parser[T]]): ctx.Expr[Parser[Probe[T]]] = {
		import ctx.universe._

		val tpe: Type = ctx.weakTypeTag[T].tpe.dealias;

		ctx.info(ctx.enclosingPosition, s"Parsing Probe[$tpe]:\n${whiteBoxCommon.showOpenImplicitsAndMacros(ctx)}", force = false);

		val body =
			q"""
import _root_.jsfacile.read.{Parser, Cursor};
import _root_.jsfacile.macros.Probe;

new Parser[Probe[$tpe]] {
	override def parse(cursor: Cursor): Probe[$tpe] = Probe($pt.parse(cursor));
}"""

		ctx.Expr[Parser[Probe[T]]](body)
	}
}