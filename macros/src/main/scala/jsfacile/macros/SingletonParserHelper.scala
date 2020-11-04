package jsfacile.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.read.Parser

trait SingletonParserHelper[S] {
	def instance: S;
}

object SingletonParserHelper {


	implicit def materialize[S]: SingletonParserHelper[S] = macro materializeImpl[S]

	def materializeImpl[S: ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[SingletonParserHelper[S]] = {
		import ctx.universe._

		val singletonType: Type = ctx.weakTypeTag[S].tpe.dealias;
		val singletonSymbol: Symbol = singletonType.typeSymbol;
		if (singletonSymbol.isModuleClass) {
			val helper =
				q"""
import _root_.jsfacile.macros.SingletonParserHelper

new SingletonParserHelper[$singletonType] {
	override def instance: $singletonType = ${singletonSymbol.asClass.module};
}"""
			// ctx.info(ctx.enclosingPosition, "sph helper: " + show(helper), false)

			ctx.Expr[SingletonParserHelper[S]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$singletonSymbol is not a module class")
		}
	}
}
