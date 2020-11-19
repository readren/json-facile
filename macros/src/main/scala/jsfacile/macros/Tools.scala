package jsfacile.macros

import scala.reflect.macros.whitebox

object Tools {

	def clearAppenderBufferOf[T]: Unit = macro clearAppenderBufferOfImpl[T];

	def clearAppenderBufferOfImpl[T : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Unit] = {
		import ctx.universe._

		val tpe: Type = ctx.weakTypeTag[T].tpe.dealias;

		val body = appenderHandlersMap.get(new TypeKey(tpe)) match {
			case Some(handler) =>
				val typeIndex = handler.typeIndex;
				q"""
import _root_.jsfacile.macros.ProductAppender.{PaLazy, productsAppendersBuffer};
import _root_.jsfacile.macros.CoproductAppenderHelper.{CaHelperLazy, caHelpersBuffer};
import _root_.jsfacile.macros.appendersBufferSemaphore;

appendersBufferSemaphore.synchronized {
	if ($typeIndex < caHelpersBuffer.size) {
		caHelpersBuffer.update($typeIndex, new CaHelperLazy);
		productsAppendersBuffer.update($typeIndex, new PaLazy);
	}
}
""";
				case None =>
				q"";
		}
		ctx.Expr[Unit](body);
	}

}
