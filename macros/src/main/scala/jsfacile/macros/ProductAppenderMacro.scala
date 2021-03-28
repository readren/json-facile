package jsfacile.macros

import scala.reflect.macros.blackbox

import jsfacile.macros.GenCommon.TypeKey
import jsfacile.write
import jsfacile.joint.{DiscriminatorDecider, DiscriminatorValueMapper, ProductsOnly}
import jsfacile.write.Appender

class ProductAppenderMacro[P, Ctx <: blackbox.Context](context: Ctx) extends AppenderGenCommon(context) {
	import ctx.universe._

	def materializeImpl(productType: Type, productSymbol: ClassSymbol): ctx.Expr[Appender[P]] = {

		//	ctx.info(ctx.enclosingPosition, s"product appender start for ${show(productType)}", force = false)

		val isOuterMacroInvocation = isOuterAppenderMacroInvocation;
		if (isOuterMacroInvocation) {
			/** Discard the appenders generated in other code contexts. This is necessary because: (1) Since the existence of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] the derived [[Appender]]s depends on the context; and (2) the existence of an [[Appender]] in the implicit scope depends on the context. */
			Handler.appenderHandlersMap.clear()
		}

		val productTypeKey = new TypeKey(productType);
		val productHandler = Handler.appenderHandlersMap.get(productTypeKey) match {

			case None =>
				val paTypeIndex = Handler.appenderHandlersMap.size;
				val productHandler = new Handler(paTypeIndex);
				Handler.registerAppenderDependency(productHandler);
				Handler.appenderHandlersMap.put(productTypeKey, productHandler);

				val paramsList = productSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

				var isFirstField = true;
				val appendProductFields_codeLines =
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
								Handler.appenderHandlersMap.get(new TypeKey(paramType)) match {
									case Some(paramHandler) =>
										productHandler.addDependency(paramHandler);
										Some(q"""appendersBuffer(${paramHandler.typeIndex}).get[$paramType]""")

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

				val prefixInserterType = appliedType(typeOf[write.PrefixInserter[_, _]].typeConstructor, List(productType, typeOf[ProductsOnly]));
				val prefixInserterInstance = ctx.inferImplicitValue(prefixInserterType, silent = true, withMacrosDisabled = true);

				val createAppenderCodeLines = {
					val appendNonDiscriminatorFields_codeLines = {
						if (prefixInserterInstance == EmptyTree) {
							q"..$appendProductFields_codeLines"
						} else if (appendProductFields_codeLines.isEmpty) {
							q"$prefixInserterInstance.insert(r, p, false, ${productSymbol.name.toString})"
						}
						else {
							q"""
if($prefixInserterInstance.insert(r, p, false, ${productSymbol.name.toString})) {
	r.append(',');
}
..$appendProductFields_codeLines"""
						}
					}

					val discriminatorDeciderType = appliedType(typeOf[DiscriminatorDecider[_, _]].typeConstructor, List(productType, typeOf[ProductsOnly]));
					val discriminatorDeciderInstance = ctx.inferImplicitValue(discriminatorDeciderType, silent = true, withMacrosDisabled = true);

					val appendDiscriminatorField_codeLine = {
						if (discriminatorDeciderInstance == EmptyTree) {
							q"r.append('{');"
						} else {
							val discriminatorValueMapperType = appliedType(typeOf[DiscriminatorValueMapper[_, _]].typeConstructor, List(productType, typeOf[ProductsOnly]));
							val discriminatorValueMapperInstance = ctx.inferImplicitValue(discriminatorValueMapperType, silent = true, withMacrosDisabled = true);

							val tail =
								if (appendNonDiscriminatorFields_codeLines == EmptyTree) {
									"\""
								} else {
									"\","
								}

							if (discriminatorValueMapperInstance == EmptyTree) {
								val body = s"""":"${productSymbol.name.toString}$tail"""
								q"""r.append("{\"").append($discriminatorDeciderInstance.fieldName).append($body)"""
							} else {
								val value = q"$discriminatorValueMapperInstance.apply(${productSymbol.name.toString})";
								q"""r.append("{\"").append($discriminatorDeciderInstance.fieldName).append("\":\"").append($value).append($tail);"""
							}
						}
					}

					q"""
new Appender[$productType] {
	override def append(r: Record, p: $productType): Record = {
  		$appendDiscriminatorField_codeLine
		$appendNonDiscriminatorFields_codeLines
		r.append('}');
	}
}"""
				}

				val createAppenderCodeLinesWithContext =
					q"""
import _root_.scala.Array;
import _root_.jsfacile.macros.LazyAppender;
import _root_.jsfacile.write.{Appender, Record};

(appendersBuffer: Array[LazyAppender]) => $createAppenderCodeLines""";

				//	ctx.info(ctx.enclosingPosition, s"product appender unchecked builder for ${show(productType)} :\n${show(createAppenderCodeLines)}\n------${Handler.showAppenderDependencies(productHandler)}\n$showEnclosingMacros", force = false);
				productHandler.creationTreeOrErrorMsg = Some(Right(createAppenderCodeLines));
				// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[appenderHandlersMap]], and this macro execution needs to know of them later.
				expandNestedMacros(createAppenderCodeLinesWithContext /*.duplicate*/);
				productHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				//	ctx.info(ctx.enclosingPosition, s"product appender after builder check for ${show(productType)}", force = false);

				productHandler

			case Some(productHandler) =>
				Handler.registerAppenderDependency(productHandler)
				productHandler
		}

		buildBody[P](productType, productHandler, isOuterMacroInvocation);
	}
}
