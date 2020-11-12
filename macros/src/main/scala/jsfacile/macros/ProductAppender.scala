package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.joint.ProductUpperBound
import jsfacile.write.Appender

object ProductAppender {

	private val cache: mutable.HashMap[whitebox.Context#Type, whitebox.Context#Tree] = mutable.HashMap.empty

	/** Concrete classes (including singleton) for which the [[jsfacile.write]] package provides an implicit [[Appender]]. */
	val concreteClassesForWhichTheWritePackageProvidesAnImplicitAppender: Set[String] = Set(
		classOf[java.lang.String].getName, // by jaString
		classOf[scala.BigInt].getName, // by jaBigInt
		classOf[scala.BigDecimal].getName, // by jaBigDecimal
		classOf[scala.Option[Any]].getName, // by jaSome and by jaNone
		classOf[scala.collection.Iterable[Any]].getName, // by jaIterable
		classOf[scala.collection.Map[Any, Any]].getName, // by jaUnsortedMap
		classOf[scala.collection.SortedMap[_, Any]].getName // by jaSortedMap
	);

	def materializeImpl[P <: ProductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[P]] = {
		import ctx.universe._

		def doesTheWritePackageProvideAnImplicitAppenderFor(classSymbol: ClassSymbol): Boolean = {
			classSymbol.baseClasses.exists(bc => concreteClassesForWhichTheWritePackageProvidesAnImplicitAppender.contains(bc.fullName))
		}

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (!productSymbol.isClass || productSymbol.isAbstract) {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol is not a concrete class")
		}
		val classSymbol = productSymbol.asClass;
		if (doesTheWritePackageProvideAnImplicitAppenderFor(classSymbol)) {
			ctx.abort(ctx.enclosingPosition, s"""An appender for $classSymbol is already provided in the "jsfacile.write" package.""")
		}

		ctx.info(ctx.enclosingPosition, s"product appender helper start for ${show(productType)}", true)

		val helper = cache.getOrElseUpdate(
		productType, {
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
		ctx.info(ctx.enclosingPosition, s"product appender helper end for ${show(productType)}: ${show(helper)}", true)

		ctx.Expr[Appender[P]](ctx.typecheck(helper));
	}
}
