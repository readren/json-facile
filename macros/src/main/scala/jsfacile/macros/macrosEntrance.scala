package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.api.builder.{ProductAppendingInfo, ProductParsingInfo}
import jsfacile.read.{Parser, SingletonParser}
import jsfacile.write.Appender


object macrosEntrance {

	def materializeAppenderImpl[T : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Appender[T]] = {
		import ctx.universe._

		val customType: Type = ctx.weakTypeTag[T].tpe.dealias;
		val customSymbol: Symbol = customType.typeSymbol;
		if (!customSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$customSymbol is not a class")
		}
		if(customSymbol.isAbstract) {
			val coproductAppenderMacro = new CoproductAppenderMacro[T, ctx.type](ctx);
			coproductAppenderMacro.materializeImpl(customType, customSymbol.asClass)
		} else {
			val productAppenderMacro = new ProductAppenderMacro[T, ctx.type](ctx);
			productAppenderMacro.materializeImpl(customType, customSymbol.asClass);
		}
	}


	def materializeParserImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Parser[T]] = {
		import ctx.universe._

		val customType: Type = ctx.weakTypeTag[T].tpe.dealias;
		val customSymbol: Symbol = customType.typeSymbol;

		if (!customSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$customSymbol is not a class.")
		}

		val customClassSymbol = customSymbol.asClass;
		if (customClassSymbol.isAbstract) {
			val coproductParserMacro = new CoproductParserMacro[T, ctx.type](ctx);
			coproductParserMacro.materializeImpl(customType, customClassSymbol);

		} else if (customClassSymbol.isModuleClass) {
			val body = q"""new _root_.jsfacile.read.SingletonParser[$customType](${customClassSymbol.module})"""
			ctx.Expr[SingletonParser[T]](body);

		} else {
			val productParserMacro = new ProductParserMacro[T, ctx.type](ctx);
			productParserMacro.materializeImpl(customType, customClassSymbol)
		}
	}

	//////////////////////////

	def addFieldToParsingInfo[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag, F: ctx.WeakTypeTag](ctx: blackbox.Context)(name: ctx.Expr[String]): ctx.Expr[Unit] = {
		val builder = new ParserBuilderMacro[C, ctx.type](ctx);
		builder.addFieldToParsingInfo[P, F](ctx.weakTypeOf[C], ctx.weakTypeOf[P], ctx.weakTypeOf[F], name, None)
	}

	def addFieldToParsingInfoWithDefaultValue[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag, F: ctx.WeakTypeTag](ctx: blackbox.Context)(name: ctx.Expr[String], defaultValue: ctx.Expr[F]): ctx.Expr[Unit] = {
		val builder = new ParserBuilderMacro[C, ctx.type](ctx);
		builder.addFieldToParsingInfo[P, F](ctx.weakTypeOf[C], ctx.weakTypeOf[P], ctx.weakTypeOf[F], name, Some(defaultValue))
	}

	def completeProductParsingInfo[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(ctor: ctx.Expr[Seq[Any] => P]): ctx.Expr[ProductParsingInfo[P]] = {
		val builder = new ParserBuilderMacro[C, ctx.type](ctx);
		builder.completeProductParsingInfo[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], ctor, None)
	}

	def completeProductParsingInfoSpecifyingDiscriminatorValue[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(discriminatorValue: ctx.Expr[String])(ctor: ctx.Expr[Seq[Any] => P]): ctx.Expr[ProductParsingInfo[P]] = {
		val builder = new ParserBuilderMacro[C, ctx.type](ctx);
		builder.completeProductParsingInfo[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], ctor, Some(discriminatorValue))
	}

	def addFieldToAppendingInfo[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag, F: ctx.WeakTypeTag](ctx: blackbox.Context)(name: ctx.Expr[String], accessor: ctx.Expr[P => F]): ctx.Expr[Unit] = {
		val builder = new AppenderBuilderMacro[C, ctx.type](ctx);
		builder.addFieldToAppendingInfo[P, F](ctx.weakTypeOf[C], ctx.weakTypeOf[P], ctx.weakTypeOf[F], name, accessor)
	}

	def completeProductAppendingInfo[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[ProductAppendingInfo[P]] = {
		val builder = new AppenderBuilderMacro[C, ctx.type](ctx);
		builder.completeProductAppendingInfo[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], None)
	}
	def completeProductAppendingInfoSpecifyingDiscriminatorValue[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(discriminatorValue: ctx.Expr[String]): ctx.Expr[ProductAppendingInfo[P]] = {
		val builder = new AppenderBuilderMacro[C, ctx.type](ctx);
		builder.completeProductAppendingInfo[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], Some(discriminatorValue))
	}

	def addCase[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Unit] = {
		val builder = new GenCommon[ctx.type](ctx);
		builder.addCase[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], None, None)
	}

	def addCaseWithAppender[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(appendingInfo: ctx.Expr[ProductAppendingInfo[P]]): ctx.Expr[Unit] = {
		val builder = new GenCommon[ctx.type](ctx);
		builder.addCase[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], Some(appendingInfo), None)
	}

	def addCaseWithParser[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(parsingInfo: ctx.Expr[ProductParsingInfo[P]]): ctx.Expr[Unit] = {
		val builder = new GenCommon[ctx.type](ctx);
		builder.addCase[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], None, Some(parsingInfo))
	}

	def addCaseWithBoth[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context)(appendingInfo: ctx.Expr[ProductAppendingInfo[P]], parsingInfo: ctx.Expr[ProductParsingInfo[P]]): ctx.Expr[Unit] = {
		val builder = new GenCommon[ctx.type](ctx);
		builder.addCase[P](ctx.weakTypeOf[C], ctx.weakTypeOf[P], Some(appendingInfo), Some(parsingInfo))
	}

	def sealParser[C: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Parser[C]] = {
		val builder = new ParserBuilderMacro[C, ctx.type](ctx);
		builder.sealParser(ctx.weakTypeOf[C])
	}

	def sealAppender[C: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Appender[C]] = {
		val builder = new AppenderBuilderMacro[C, ctx.type](ctx);
		builder.sealAppender(ctx.weakTypeOf[C])
	}
}
