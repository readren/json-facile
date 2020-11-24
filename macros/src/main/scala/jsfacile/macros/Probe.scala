package jsfacile.macros

import scala.reflect.macros.whitebox

import jsfacile.write.Appender

trait Probe[T] {
	def value: T
}

object Probe {

	implicit def materializeAppender[T]: Appender[Probe[T]] = macro materializeAppenderImpl[T]

	def materializeAppenderImpl[T : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[Probe[T]]] = {
		import ctx.universe._

		val tpe: Type = ctx.weakTypeTag[T].tpe.dealias;

		ctx.info(ctx.enclosingPosition, s"Probe on $tpe:\n${showOpenImplicitsAndMacros(ctx)}", force = false);

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
}