package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.write.Appender

object CustomAppenderMacro {

	def materializeImpl[T : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Appender[T]] = {
		import ctx.universe._

		val customType: Type = ctx.weakTypeTag[T].tpe.dealias;
		val customSymbol: Symbol = customType.typeSymbol;
		if (!customSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$customSymbol is not a class")
		}
		if(customSymbol.isAbstract) {
			val coproductAppenderMacro = new CoproductAppenderMacro[ctx.type](ctx);
			coproductAppenderMacro.materializeImpl[T](customType, customSymbol.asClass)
	 	} else {
			val productAppenderMacro = new ProductAppenderMacro[ctx.type](ctx);
			productAppenderMacro.materializeImpl[T](customType, customSymbol.asClass);
		}
	}
}
