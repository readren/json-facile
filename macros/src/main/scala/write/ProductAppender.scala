package write

import scala.reflect.macros.blackbox


object ProductAppender {

	type UpperBound = Product

//	implicit def materialize[P <: UpperBound]: Appender[P] = macro materializeImpl[P];

	def materializeImpl[P <: UpperBound : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Appender[P]] = {
		import ctx.universe._

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (productSymbol.isClass && !productSymbol.isAbstract ) {
			val classSymbol = productSymbol.asClass;
			val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

			var isFirstField = true;
			val appendFieldSnippets =
				for {
					params <- paramsList
					param <- params
				} yield {
					val start = {
						if(isFirstField) {
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

			val helper = q"""
import _root_.write._;
new Appender[${productType}] {
	override def append(r: Record, p: $productType): Record = {
		r.append('{');
		..${appendFieldSnippets}
  		r.append('}');
	}
}""";

			ctx.info(ctx.enclosingPosition, "ProductAppender=" + show(helper), false);
			ctx.Expr[Appender[P]](ctx.typecheck(helper));
		} else {
			ctx.warning(ctx.enclosingPosition, s"$productSymbol is not a class and only classes are supported")
			ctx.Expr[Appender[P]](q"")
		}
	}
}
