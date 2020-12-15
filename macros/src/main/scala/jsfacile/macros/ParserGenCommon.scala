package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.read.Parser

class ParserGenCommon[Ctx <: blackbox.Context](context: Ctx) extends GenCommon(context) {
	import ctx.universe._

	def buildBody[T](initialType: Type, initialHandler: Handler): ctx.Expr[Parser[T]] = {

		val body =
			if (initialHandler.creationTreeOrErrorMsg.isDefined && this.isOuterParserMacroInvocation) {
				val inits =
					for {
						(innerTypeKey, innerHandler) <- parserHandlersMap
						if initialHandler.doesDependOn(innerHandler.typeIndex)
					} yield {
						innerHandler.creationTreeOrErrorMsg match {
							case Some(Right(parserCreationTree)) =>
								q"""
val parserCreator = ${parserCreationTree.asInstanceOf[Tree]};
parsersBuffer(${innerHandler.typeIndex}).set(parserCreator.apply(parsersBuffer));"""

							case Some(Left(innerErrorMsg)) =>
								ctx.abort(ctx.enclosingPosition, s"""Unable to derive a parser for `$initialType` because it depends on the parser for `$innerTypeKey` whose derivation has failed saying: $innerErrorMsg""")

							case None =>
								ctx.abort(ctx.enclosingPosition, s"Unreachable reached: initialType=$initialType, innerType=$innerTypeKey\n${showParserDependencies(initialHandler)}\n$showEnclosingMacros")
						}
					}

				q"""
import _root_.jsfacile.macros.LazyParser;

val parsersBuffer = _root_.scala.Array.fill(${parserHandlersMap.size})(new LazyParser);
{..$inits}
parsersBuffer(${initialHandler.typeIndex}).get[$initialType]"""

			} else {
				q"""parsersBuffer(${initialHandler.typeIndex}).get[$initialType]"""
			}

		ctx.info(ctx.enclosingPosition, s"Parser body for ${show(initialType)}: ${show(body)}\n------${showParserDependencies(initialHandler)}\n$showEnclosingMacros", force = false);

		ctx.Expr[Parser[T]](body);
	}

	def isOuterParserMacroInvocation: Boolean = {
		this.isOuterMacroInvocation { methodName =>
			methodName == "jsfacile.read.PriorityLowParsers.jpCustom" ||
			methodName == "jsfacile.api.CoproductBuilder.parser"
		}
	}
}
