package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.write.Appender

class ProductAppenderMacro[P, Ctx <: blackbox.Context](context: Ctx) extends AppenderGenCommon(context) {
	import ctx.universe._

	def materializeImpl(productType: Type, productSymbol: ClassSymbol): ctx.Expr[Appender[P]] = {

//		ctx.info(ctx.enclosingPosition, s"product appender start for ${show(productType)}", force = false)

		val productTypeKey = new TypeKey(productType);
		val productHandler = appenderHandlersMap.get(productTypeKey) match {

			case None =>
				val paTypeIndex = appenderHandlersMap.size;
				val productHandler = new Handler(paTypeIndex);
				registerAppenderDependency(productHandler);
				appenderHandlersMap.put(productTypeKey, productHandler);

				val paramsList = productSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

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

										if (!paramTypeSymbol.isAbstract || paramTypeSymbol.asClass.isSealed) {
											Some(q"""appendersBuffer(${paramHandler.typeIndex}).get[$paramType]""")
										} else {
											val msg = s"Unreachable reached: productType=$productType, paramType=$paramType"
											productHandler.setFailed(msg);
											ctx.abort(ctx.enclosingPosition, msg)
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

				val createAppenderCodeLines =
					q"""
import _root_.scala.Array;
import _root_.jsfacile.macros.LazyAppender;
import _root_.jsfacile.write.{Appender, Record};

val createAppender: Array[LazyAppender] => Appender[$productType] = appendersBuffer =>
	new Appender[$productType] {
		override def append(r: Record, p: $productType): Record = {
			r.append('{')
			..$appendField_codeLines
			r.append('}');
		}
	}
createAppender""";

				ctx.info(ctx.enclosingPosition, s"product appender unchecked builder for ${show(productType)} : ${show(createAppenderCodeLines)}\n------${showAppenderDependencies(productHandler)}\n$showEnclosingMacros", force = false);
				productHandler.creationTreeOrErrorMsg = Some(Right(createAppenderCodeLines));
				// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[appenderHandlersMap]], and this macro execution needs to know of them later.
				ctx.typecheck(createAppenderCodeLines);
				productHandler.isCapturingDependencies = false;  // this line must be immediately after the manual type-check
				ctx.info(ctx.enclosingPosition, s"product appender after builder check for ${show(productType)}", force = false);

				productHandler

			case Some(productHandler) =>
				registerAppenderDependency(productHandler)
				productHandler
		}

		buildBody[P](productType, productHandler);
	}
}
