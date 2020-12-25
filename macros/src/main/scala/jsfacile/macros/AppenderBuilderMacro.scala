package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.macros.GenCommon.{ProductCustomization, TypeKey}
import jsfacile.macros.Handler.appenderHandlersMap
import jsfacile.write.Appender

class AppenderBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductAppenderMacro(context) {
	import ctx.universe._

	def sealAppender(coproductType: Type): ctx.Expr[Appender[C]] = {
		val initialCoproductSymbol = getClassSymbol(coproductType);

		val coproductTypeKey = new TypeKey(coproductType);
		val keeper = getCoproductBuilderStateOf(coproductTypeKey);

		val coproductHandler = Handler.getCleanHandlerFor(coproductTypeKey, appenderHandlersMap)

		val productsInfoCollector: mutable.ArrayBuffer[ProductInfo] = mutable.ArrayBuffer.empty;
		for {(productType, productConfig) <- keeper.productsCollector.asInstanceOf[mutable.Map[Type, ProductCustomization]]} {
			productConfig.oAppendingInfo match {
				case None =>
					val productSymbol = productType.typeSymbol;
					this.addProduct(productSymbol.asClass, initialCoproductSymbol, coproductType, coproductHandler, productsInfoCollector);

				case Some(productInfo) =>
					productsInfoCollector.addOne(CustomProductInfo(productType, productInfo.requiredFieldNames, productInfo.appenderTree.asInstanceOf[Tree]))
			}
		}

		coproductHandler.creationTreeOrErrorMsg match {
			case None =>
				buildAppenderCreationTreeOn(coproductType, coproductHandler, None, productsInfoCollector);
				this.buildBody[C](coproductType, coproductHandler, isOuterMacroInvocation = true);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")

		}
	}
}
