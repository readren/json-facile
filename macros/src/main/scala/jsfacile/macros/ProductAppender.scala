package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.write.{Appender, Record}

object ProductAppender {

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


	class PaLazy extends Appender[ProductUpperBound] with Lazy {
		@volatile private var instance: Appender[ProductUpperBound] = _;
		def set[P <: ProductUpperBound](appender: Appender[P]): Unit = this.instance = appender.asInstanceOf[Appender[ProductUpperBound]];
		def get[P <: ProductUpperBound]: Appender[P] = this.instance.asInstanceOf[Appender[P]];
		override def isEmpty: Boolean = this.instance == null;
		override def append(record: Record, a: ProductUpperBound): Record = this.instance.append(record, a);
	}

	val productsAppendersBuffer: mutable.ArrayBuffer[PaLazy] = mutable.ArrayBuffer.empty;

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

		ctx.info(ctx.enclosingPosition, s"product appender start for ${show(productType)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false)

		val productTypeKey = new TypeKey(productType);
		val productHandler = appenderHandlersMap.get(productTypeKey) match {

			case None =>
				val productTypeIndex = appenderHandlersMap.size;
				val productHandler = new Handler(productTypeIndex);
				registerAppenderDependency(productHandler);
				appenderHandlersMap.put(productTypeKey, productHandler);

				val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

				var isFirstField = true;
				val appendField_codeLines =
					for {
						params <- paramsList
						param <- params
					} yield {
						val paramNameStr = param.name.decodedName.toString;
						val sb = new StringBuilder(paramNameStr.length + 4)
						if (isFirstField) {
							isFirstField = false
						} else {
							sb.append(',');
						}
						sb.append('"').append(paramNameStr).append('"').append(':');

						val paramType = param.typeSignature.dealias;
						val paramTypeSymbol = paramType.typeSymbol;

						val oGetAlreadyExpandedAppenderExpression =
							if (paramTypeSymbol.isClass) { // if the field type is a class or trait
								appenderHandlersMap.get(new TypeKey(paramType)) match {
									case Some(paramHandler) =>
										productHandler.addDependency(paramHandler);

										if (!paramTypeSymbol.isAbstract) { // if the field type is a concrete class, including module classes (scala objects)
											Some(q"productsAppendersBuffer(${paramHandler.typeIndex}).get[$paramType]")

										} else if (paramTypeSymbol.asClass.isSealed) { // if the field type is a trait or abstract class
											Some(
												q"""new _root_.jsfacile.write.CoproductAppender[$paramType](caHelpersBuffer(${paramHandler.typeIndex}).get[$paramType])""")
										} else {
											ctx.abort(ctx.enclosingPosition, "Unreachable")
										}

									case None =>
										None
								}
							} else {
								None
							}

						oGetAlreadyExpandedAppenderExpression match {
							case Some(appenderExpression) =>
								q"""r.append(${sb.toString}).appendSummoned[$paramType](${Select(Ident(TermName("p")), param.name)})($appenderExpression);""";

							case None =>
								q"""r.append(${sb.toString}).appendSummoned[$paramType](${Select(Ident(TermName("p")), param.name)})""";
						}

					}

				val productAppenderExpression =
					q"""
import _root_.jsfacile.macros.CoproductAppenderHelper.caHelpersBuffer;
import _root_.jsfacile.macros.ProductAppender.productsAppendersBuffer;
import _root_.jsfacile.write.{Appender, Record};

val proxy = productsAppendersBuffer($productTypeIndex);
if (proxy.isEmpty) {
	proxy.set(new Appender[$productType] {
		override def append(r: Record, p: $productType): Record = {
			r.append('{')
			..$appendField_codeLines
			r.append('}');
		}
	});
}""";

				ctx.info(ctx.enclosingPosition, s"product appender unchecked init for ${show(productType)} : ${show(productAppenderExpression)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);
				// the recursion is triggered by the type-check
        ctx.typecheck(productAppenderExpression)
				productHandler.oExpression = Some(ctx.Expr[Unit](productAppenderExpression))
				productHandler.isCapturingDependencies = false
				ctx.info(ctx.enclosingPosition, s"product appender after init check for ${show(productType)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);

				productHandler

			case Some(productHandler) =>
				registerAppenderDependency(productHandler)
				productHandler
		}

		val body =
			if (productHandler.oExpression.isDefined && isOuterAppenderMacroInvocation(ctx)) {
				val inits =
					for {
						(_, handler) <- appenderHandlersMap
						if productHandler.doesDependOn(handler.typeIndex)
					} yield handler.oExpression.get.in(ctx.mirror);

				q"""
import _root_.jsfacile.macros.ProductAppender.{PaLazy, productsAppendersBuffer};
import _root_.jsfacile.macros.CoproductAppenderHelper.{CaHelperLazy, caHelpersBuffer};
import _root_.jsfacile.macros.appendersBufferSemaphore;

if (productsAppendersBuffer.size < ${appenderHandlersMap.size}) {
	appendersBufferSemaphore.synchronized {
		while(caHelpersBuffer.size < ${appenderHandlersMap.size}) {
			caHelpersBuffer.addOne(new CaHelperLazy);
		}
		while(productsAppendersBuffer.size < ${appenderHandlersMap.size}) {
			productsAppendersBuffer.addOne(new PaLazy);
		}
	}
}
{..$inits}
productsAppendersBuffer(${productHandler.typeIndex}).get[$productType]""";

			} else {
				q"""
import _root_.jsfacile.macros.ProductAppender.productsAppendersBuffer;
productsAppendersBuffer(${productHandler.typeIndex}).get[$productType]"""

			}

		ctx.info(ctx.enclosingPosition, s"product appender unchecked body for ${show(productType)}: ${show(body)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false)

		ctx.Expr[Appender[P]](body)
	}
}
