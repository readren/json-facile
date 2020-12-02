package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.joint.{CoproductUpperBound, discriminatorField}
import jsfacile.write.Appender

object CoproductAppenderMacro {

	def materializeImpl[C <: CoproductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Appender[C]] = {
		import ctx.universe._

		case class ProductInfo(simpleName: String, tpe: Type, requiredFieldNames: Set[String], appendField_codeLines: List[Tree]) {
			var isAmbiguous = false;
		}

		def addProductsBelongingTo(
			coproductClassSymbol: ClassSymbol,
			coproductType: Type,
			superHandler: Handler,
			productsInfoBuilder: mutable.Builder[ProductInfo, IndexedSeq[ProductInfo]]
		): Unit = {
			for {
				productSymbol <- coproductClassSymbol.knownDirectSubclasses.toIndexedSeq
			} {
				val productClassSymbol = productSymbol.asClass;
				ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productClassSymbol.toTypeConstructor) match {
					case Right(productType) =>
						if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and therefore not considered by the ambiguity detector below.

							if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields
								productsInfoBuilder.addOne(ProductInfo(
									productClassSymbol.name.toString,
									productClassSymbol.toType,
									Set.empty,
									Nil
								))

							} else if (productSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
								if (productClassSymbol.isSealed) {
									addProductsBelongingTo(productClassSymbol, productType, superHandler, productsInfoBuilder)
								} else {
									ctx.abort(ctx.enclosingPosition, s"$productClassSymbol should be sealed")
								}

							} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose fields are the parameters of said subclass primary constructor.
								val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

								val requiredFieldNamesBuilder = Set.newBuilder[String];
								var isFirstField = true;
								val appendField_codeLines =
									for {
										params <- productCtorParamsLists
										param <- params
									} yield {
										val paramNameStr = param.name.decodedName.toString;
										val sb = new StringBuilder(paramNameStr.length + 4)
										if (isFirstField) {
											isFirstField = false
										} else {
											sb.append(',');
										}

										val paramType = param.typeSignature.dealias;
										val paramTypeSymbol = paramType.typeSymbol;
										if (paramTypeSymbol.fullName != "scala.Option") {
											requiredFieldNamesBuilder.addOne(paramNameStr)
										}

										val oGetAlreadyExpandedAppenderExpression =
											if (paramTypeSymbol.isClass) {
												appenderHandlersMap.get(new TypeKey(paramType)) match {
													case Some(paramHandler) =>
														superHandler.addDependency(paramHandler);

														if (!paramTypeSymbol.isAbstract || paramTypeSymbol.asClass.isSealed) {
															Some(q"""appendersBuffer(${paramHandler.typeIndex}).get[$paramType]""")
														} else {
															ctx.abort(ctx.enclosingPosition, "Unreachable")
														}
													case None =>
														None
												}
											} else {
												None
											}
										sb.append('"').append(paramNameStr).append('"').append(':');

										oGetAlreadyExpandedAppenderExpression match {
											case Some(appenderExpression) =>
												q"""r.append(${sb.toString}).appendSummoned[$paramType](${Select(Ident(TermName("p")), param.name)})($appenderExpression);""";

											case None =>
												q"""r.append(${sb.toString}).appendSummoned[$paramType](${Select(Ident(TermName("p")), param.name)})""";
										}

									}

								productsInfoBuilder.addOne(ProductInfo(
									productSymbol.name.toString,
									productType,
									requiredFieldNamesBuilder.result(),
									appendField_codeLines
								))
							}

						}

					case Left(freeTypeParams) =>
						ctx.abort(ctx.enclosingPosition, s"""The "$productSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}.""")
				}
			}
		}

		//// the body of this method starts here ////

		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (!coproductSymbol.isClass || !coproductSymbol.isAbstract) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a trait or abstract class")
		}
		val coproductClassSymbol = coproductSymbol.asClass;
		if (!coproductClassSymbol.isSealed) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a sealed")
		}

		ctx.info(ctx.enclosingPosition, s"coproduct appender start for ${show(coproductType)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductHandler = appenderHandlersMap.get(coproductTypeKey) match {

			case None =>
				val caTypeIndex = appenderHandlersMap.size;
				val coproductHandler = new Handler(caTypeIndex)
				registerAppenderDependency(coproductHandler);
				appenderHandlersMap.put(coproductTypeKey, coproductHandler);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val (discriminatorFieldName, discriminatorIsRequired) = discriminatorField.parse(ctx.universe)(coproductClassSymbol)

				val productsInfoBuilder = IndexedSeq.newBuilder[ProductInfo]
				addProductsBelongingTo(coproductClassSymbol, coproductType, coproductHandler, productsInfoBuilder);
				val productsInfo = productsInfoBuilder.result();

				// Set the `isAmbiguous` flag to every product whose required field names match those of another product.
				if (!discriminatorIsRequired) {
					var i = productsInfo.size - 1;
					while (i > 0) {
						val pi = productsInfo(i);
						if (!pi.isAmbiguous) {
							var j = i - 1;
							while (j >= 0) {
								val pj = productsInfo(j);
								if (pi.requiredFieldNames == pj.requiredFieldNames) {
									pi.isAmbiguous = true;
									pj.isAmbiguous = true;
								}
								j -= 1;
							}
						}
						i -= 1
					}
				}

				// for every product, generate the code lines that creates the [[CahProductInfo]] and adds it to the `productsInfoBuilder`
				val addProductInfo_codeLines: Seq[ctx.universe.Tree] =
					for {
						productInfo <- productsInfo
					} yield {
						val appendDiscriminator_codeLine =
							if (discriminatorIsRequired || productInfo.isAmbiguous) {
								val discriminatorField = s""""$discriminatorFieldName":"${productInfo.simpleName}"${if (productInfo.appendField_codeLines.isEmpty) "" else ","}"""
								q"r.append($discriminatorField)"
							} else {
								q""
							}
						val productClassNameAtRuntime = productInfo.tpe.erasure.typeSymbol.fullName;
						q"""
val productAppender: _root_.jsfacile.write.Appender[${productInfo.tpe}] = { (r, p) =>
	r.append('{')

	$appendDiscriminator_codeLine

	..${productInfo.appendField_codeLines}

	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""
					}

				val createAppenderCodeLines =
					q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.jsfacile.write.CoproductAppender;
import CoproductAppender.{CahProductInfo, productInfoComparator};
import _root_.jsfacile.macros.LazyAppender;

val createAppender: Array[LazyAppender] => CoproductAppender[$coproductType] = appendersBuffer => {
	val productsInfoBuilder = ArrayBuffer[CahProductInfo[_ <: $coproductType]]();

	..$addProductInfo_codeLines

	val productsArray = productsInfoBuilder.toArray.asInstanceOf[Array[CahProductInfo[$coproductType]]];
	_root_.java.util.Arrays.sort(productsArray, productInfoComparator);
	new CoproductAppender[$coproductType](${coproductType.toString}, productsArray)
};
createAppender""";

				coproductHandler.oExpression = Some(createAppenderCodeLines);

				ctx.info(ctx.enclosingPosition, s"coproduct appender unchecked init for ${show(coproductType)} : ${show(createAppenderCodeLines)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);
				ctx.typecheck(createAppenderCodeLines);
				coproductHandler.isCapturingDependencies = false;  // this line must be immediately after the manual type-check
				ctx.info(ctx.enclosingPosition, s"coproduct appender after init check for ${show(coproductType)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);

				coproductHandler

			case Some(coproductHandler) =>
				registerAppenderDependency(coproductHandler)
				coproductHandler

		};

		val body =
			if (coproductHandler.oExpression.isDefined && isOuterAppenderMacroInvocation(ctx)) {
				val inits =
					for {
						(_, handler) <- appenderHandlersMap
						if coproductHandler.doesDependOn(handler.typeIndex)
					} yield {
						val createAppenderCodeLines = handler.oExpression.get.asInstanceOf[ctx.Tree];
						q"""appendersBuffer(${handler.typeIndex}).set($createAppenderCodeLines(appendersBuffer));"""
					}

				q"""
import _root_.jsfacile.macros.LazyAppender;

val appendersBuffer = _root_.scala.Array.fill(${appenderHandlersMap.size})(new LazyAppender);
{..$inits}
appendersBuffer(${coproductHandler.typeIndex}).get[$coproductType]""";

			} else {
				q"""appendersBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""
			}

		ctx.info(ctx.enclosingPosition, s"coproduct appender unchecked body for ${show(coproductType)}: ${show(body)}\n------\nhandlers:$showAppenderHandlers\n${showOpenImplicitsAndMacros(ctx)}", force = false);

		ctx.Expr[Appender[C]](body);
	}
}



