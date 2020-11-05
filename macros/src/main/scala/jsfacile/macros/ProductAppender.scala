package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.joint.ProductUpperBound
import jsfacile.write.Appender

object ProductAppender {

	private val cache: mutable.WeakHashMap[whitebox.Context#Type, whitebox.Context#Tree] = mutable.WeakHashMap.empty

	def materializeImpl[P <: ProductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[P]] = {
		import ctx.universe._

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (productSymbol.isClass && !productSymbol.isAbstract) {
			val helper = cache.getOrElseUpdate(
			productType, {
				val classSymbol = productSymbol.asClass;
				val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

				var isFirstField = true;
				val appendField_codeLines =
					for {
						params <- paramsList
						param <- params
					} yield {
						val paramNameStr = param.name.decodedName.toString;
						val sb = new StringBuilder(paramNameStr.size + 4)
						if (isFirstField) {
							isFirstField = false
						} else {
							sb.append(',');
						}
						sb.append('"').append(paramNameStr).append('"').append(':');

						q"""r.append(${sb.toString}).appendSummoned[${param.typeSignature.dealias}](${Select(Ident(TermName("p")), param.name)})""";
					}

				q"""
import _root_.jsfacile.write.{Appender, Record};

new Appender[$productType] {
	override def append(r: Record, p: $productType): Record = {
		r.append('{')
		..$appendField_codeLines
  		r.append('}');
	}
}"""
			}).asInstanceOf[ctx.Tree];
			// ctx.info(ctx.enclosingPosition, "pa helper: " + show(helper), false)

			ctx.Expr[Appender[P]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol is not a concrete class")
		}
	}
}
