package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.write.Appender

class AppenderBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductAppenderMacro(context) {
	import ctx.universe._

	def sealAppender(coproductType: Type): ctx.Expr[Appender[C]] = {
		val initialCoproductSymbol = coproductType.typeSymbol;
		if (!initialCoproductSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"`$initialCoproductSymbol` is not a class.")
		}

		val coproductTypeKey = new TypeKey(coproductType);
		val keeper = getKeeper(coproductTypeKey);

		val coproductHandler = getCleanHandlerFor(coproductTypeKey, appenderHandlersMap)

		val productsInfoCollector: mutable.ArrayBuffer[ProductInfo] = mutable.ArrayBuffer.empty;
		for { productType <- keeper.productsCollector.asInstanceOf[mutable.Set[Type]] } {
			val productSymbol = productType.typeSymbol;
			this.addProduct(productSymbol.asClass, initialCoproductSymbol.asClass, coproductType, coproductHandler, productsInfoCollector);
		}

		coproductHandler.creationTreeOrErrorMsg match {
			case None =>
				buildAppenderCreationTreeOn(coproductType, coproductHandler, None, productsInfoCollector);
				this.buildBody[C](coproductType, coproductHandler);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")

		}
	}
}
