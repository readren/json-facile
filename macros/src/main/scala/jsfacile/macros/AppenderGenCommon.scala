package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.write.Appender

class AppenderGenCommon[Ctx <: blackbox.Context](context: Ctx) extends GenCommon(context) {
	import ctx.universe._

	def buildBody[T](initialType: Type, initialHandler: Handler): ctx.Expr[Appender[T]] = {
		val body =
			if (initialHandler.creationTreeOrErrorMsg.isDefined && isOuterAppenderMacroInvocation) {
			val inits =
				for {
					(innerTypeKey, innerHandler) <- Handler.appenderHandlersMap
					if initialHandler.doesDependOn(innerHandler.typeIndex)
				} yield {
					innerHandler.creationTreeOrErrorMsg.get match {
						case Right(appenderCreationTree) =>
							q"""
val appender = ${appenderCreationTree.asInstanceOf[Tree]}
appendersBuffer(${innerHandler.typeIndex}).set(appender);"""

						case Left(innerErrorMsg) =>
							ctx.abort(ctx.enclosingPosition, s"Unable to derive an appender for $initialType because it depends on the appender for ${innerTypeKey.toString} whose derivation has failed saying: $innerErrorMsg.")
					}
				}

			q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.write.{Appender, Record, CoproductAppender};
import CoproductAppender.{CahProductInfo, productInfoComparator};
import _root_.jsfacile.macros.LazyAppender;

val appendersBuffer = _root_.scala.Array.fill(${Handler.appenderHandlersMap.size})(new LazyAppender);
{..$inits}
appendersBuffer(${initialHandler.typeIndex}).get[$initialType]""";

		} else {
			q"""appendersBuffer(${initialHandler.typeIndex}).get[$initialType]"""
		}

		ctx.info(ctx.enclosingPosition, s"appender body for ${show(initialType)}: ${show(body)}\n------${Handler.showAppenderDependencies(initialHandler)}\n$showEnclosingMacros", force = false);

		ctx.Expr[Appender[T]](body);
	}


	def isOuterAppenderMacroInvocation: Boolean = {
		this.isOuterMacroInvocation { methodName =>
			methodName == "jsfacile.write.PriorityLowAppenders.jaCustom" ||
			methodName == "jsfacile.api.CoproductBuilder.appender"
		}
	}
}
