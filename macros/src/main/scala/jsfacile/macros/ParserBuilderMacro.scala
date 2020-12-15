package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot


class ParserBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductParserMacro(context) {
	import ctx.universe._

	def sealParser(coproductType: Type): ctx.Expr[Parser[C]] = {
		val coproductSymbol = coproductType.typeSymbol;
		if (!coproductSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"`$coproductSymbol` is not a class.")
		}
		val coproductClassSymbol = coproductSymbol.asClass;

		val coproductTypeKey = new TypeKey(coproductType);
		val keeper = getKeeper(coproductTypeKey);

		val coproductHandler = getCleanHandlerFor(coproductTypeKey, parserHandlersMap);

		val productAdditionTrees: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty;
		var nextBitSlot: BitSlot = BitSet.FIRST_BIT_SLOT;
		val metaConsideredFields = mutable.Map.empty[String, ConsideredField];
		for (productType <- keeper.productsCollector.asInstanceOf[mutable.Set[Type]]) {
			val productSymbol = productType.typeSymbol;
			nextBitSlot = this.addProduct(productSymbol.asClass, coproductClassSymbol, coproductType, coproductType, coproductHandler, nextBitSlot, metaConsideredFields, productAdditionTrees);
		}

		coproductHandler.creationTreeOrErrorMsg match {
			case None =>
				this.buildParserCreationTreeOn(coproductHandler, coproductType, coproductClassSymbol, productAdditionTrees, nextBitSlot.shardIndex + 1)
				this.buildBody[C](coproductType, coproductHandler);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")
		}
	}
}
