package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.write.Appender

object ProductAppender {

	type UpperBound = Product

	private val cache: mutable.WeakHashMap[String, whitebox.Context#Tree] = mutable.WeakHashMap.empty

	def materializeImpl[P <: UpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[P]] = {
		import ctx.universe._

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (productSymbol.isClass && !productSymbol.isAbstract) {
			val helper = cache.getOrElseUpdate(
			productSymbol.fullName, {
				val classSymbol = productSymbol.asClass;
				val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

				var isFirstField = true;
				val appendFieldSnippets =
					for {
						params <- paramsList
						param <- params
					} yield {
						val start = {
							if (isFirstField) {
								isFirstField = false;
								q"""r.append('"')"""
							} else {
								q"""r.append(",\"")"""
							}
						}
						q"""
$start
	.append(${param.name.toString})
	.append("\":")
	.appendSummoned[${param.typeSignature.dealias}](${Select(Ident(TermName("p")), param.name)})""";
					}

				q"""
import _root_.jsfacile.write.{Appender, Record};

new Appender[$productType] {
	override def append(r: Record, p: $productType): Record = {
		r.append('{');
		..$appendFieldSnippets
  		r.append('}');
	}
}"""
			}).asInstanceOf[ctx.Tree];
//			ctx.info(ctx.enclosingPosition, "pa helper: " + show(helper), false)
			ctx.Expr[Appender[P]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol should be a concrete class")
		}
	}
}
