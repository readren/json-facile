package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.read.{Parser, SingletonParser}


object CustomParserMacro {

	def materializeImpl[T : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Parser[T]] = {
		import ctx.universe._

		val customType: Type = ctx.weakTypeTag[T].tpe.dealias;
		val customSymbol: Symbol = customType.typeSymbol;

		if (!customSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$customSymbol is not a class")
		}

		val customClassSymbol = customSymbol.asClass;
		if (customClassSymbol.isAbstract) {
			val coproductParserMacro = new CoproductParserMacro[ctx.type](ctx);
			coproductParserMacro.materializeImpl[T](customType, customClassSymbol);

		} else if (customClassSymbol.isModuleClass) {
			val body = q"""new _root_.jsfacile.read.SingletonParser[$customType](${customClassSymbol.module})"""
			ctx.Expr[SingletonParser[T]](body);

		} else {
			val productParserMacro = new ProductParserMacro[ctx.type](ctx);
			productParserMacro.materializeImpl[T](customType, customClassSymbol)
		}
	}
}
