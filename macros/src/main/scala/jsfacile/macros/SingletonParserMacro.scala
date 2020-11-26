package jsfacile.macros

import scala.reflect.macros.whitebox

import jsfacile.read.SingletonParser

object SingletonParserMacro {

	def materializeImpl[S: ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[SingletonParser[S]] = {
		import ctx.universe._

		val singletonType: Type = ctx.weakTypeTag[S].tpe.dealias;
		val singletonSymbol: Symbol = singletonType.typeSymbol;
		if (singletonSymbol.isModuleClass) {
			val body = q"""new _root_.jsfacile.read.SingletonParser[$singletonType](${singletonSymbol.asClass.module})"""
			ctx.Expr[SingletonParser[S]](body);
		} else {
			ctx.abort(ctx.enclosingPosition, s"$singletonSymbol is not a module class")
		}
	}
}
