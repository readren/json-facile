package jsfacile.macros

import scala.reflect.macros.whitebox

import jsfacile.read.Parser


object EnumParserMacro {

	def materializeImpl[E <: Enumeration : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Parser[E#Value]] = {
		import ctx.universe._

		val enumType: Type = ctx.weakTypeTag[E].tpe.dealias;
		val enumModuleSymbol = enumType.termSymbol.asModule;
		val body = q"""new _root_.jsfacile.read.EnumParser[${enumModuleSymbol}](${enumModuleSymbol})""";

		ctx.Expr[Parser[E#Value]](body);
	}
}
