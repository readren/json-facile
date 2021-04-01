package jsfacile.macros

import scala.collection.mutable
import scala.collection.IndexedSeq
import scala.reflect.macros.blackbox

import jsfacile.annotations.discriminatorField
import jsfacile.joint.{CoproductsOnly, DiscriminatorConf, DiscriminatorDecider, DiscriminatorValueMapper}
import jsfacile.macros.GenCommon.TypeKey
import jsfacile.write.{Appender, PrefixInserter}

class CoproductAppenderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends AppenderGenCommon(context) {
	import ctx.universe._

	/** Knows information pertaining to a concrete subtype of `C` */
	protected trait ProductInfo {
		var isAmbiguous: Boolean;
		def tpe: Type;
		def requiredFieldNames: Set[String];
	}
	/** A [[ProductInfo]] obtained from the primary constructor of the subtype. */
	protected case class DerivedProductInfo(symbolName: String, tpe: Type, requiredFieldNames: Set[String], appendField_codeLines: Iterable[Tree]) extends ProductInfo {
		var isAmbiguous = false;
	}
	/** A [[ProductInfo]] specified by the library user by means of one of the [[jsfacile.api.builder.CoproductTranslatorsBuilder.add*]] methods that customize the parsing. */
	protected case class CustomProductInfo(tpe: Type, requiredFieldNames: Set[String], appenderTree: Tree) extends ProductInfo {
		var isAmbiguous = false;
	}

	def materializeImpl(initialCoproductType: Type, initialCoproductClassSymbol: ClassSymbol): ctx.Expr[Appender[C]] = {

		//	ctx.info(ctx.enclosingPosition, s"coproduct appender start for ${show(coproductType)}", force = false);

		val isOuterMacroInvocation = isOuterAppenderMacroInvocation;
		if (isOuterMacroInvocation) {
			/** Discard the appenders generated in other code contexts. This is necessary because: (1) Since the existence of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] the derived [[Appender]]s depends on the context; and (2) the existence of an [[Appender]] in the implicit scope depends on the context. */
			Handler.appenderHandlersMap.clear()
		}

		val coproductTypeKey = new TypeKey(initialCoproductType);
		val coproductHandler = Handler.appenderHandlersMap.get(coproductTypeKey) match {

			case None =>
				val caTypeIndex = Handler.appenderHandlersMap.size;
				val coproductHandler = new Handler(caTypeIndex)
				Handler.registerAppenderDependency(coproductHandler);
				Handler.appenderHandlersMap.put(coproductTypeKey, coproductHandler);

				if (!initialCoproductClassSymbol.isSealed) {
					val errorMsg = s"No appender for `$initialCoproductType` was found in the implicit scope, and it can't be automatically derived because `$initialCoproductClassSymbol` is not sealed.\nIf an appender for `$initialCoproductType` exists, make sure it is in the implicit scope and also that all its implicit parameters are resolvable. A common cause of this error is the failure to resolve one of said implicit parameters.\nIf your intention is to rely on the automatic derivation, seal the `$initialCoproductClassSymbol` and make all its subtypes be arithmetic.\nIf you are planning to build a custom appender, consider the use of a `CoproductTranslatorsBuilder`.";
					coproductHandler.setFailed(errorMsg)
					ctx.abort(ctx.enclosingPosition, errorMsg)
				}

				val productsInfoCollector: mutable.ArrayBuffer[ProductInfo] = mutable.ArrayBuffer.empty;
				addSubtypesOf(initialCoproductClassSymbol, initialCoproductType, coproductHandler, productsInfoCollector);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorOverride = discriminatorField.parse(ctx)(initialCoproductClassSymbol);

				buildAppenderCreationTreeOn(initialCoproductType, coproductHandler, discriminatorOverride, productsInfoCollector);

				coproductHandler

			case Some(coproductHandler) =>
				Handler.registerAppenderDependency(coproductHandler)
				coproductHandler

		};

		this.buildBody[C](initialCoproductType, coproductHandler, isOuterMacroInvocation);
	}


	protected def buildAppenderCreationTreeOn(coproductType: Type, coproductHandler: Handler, discriminatorOverride: Option[DiscriminatorConf], productsInfo: IndexedSeq[ProductInfo]): Unit = {

		// If the discriminator annotation is absent or its `required` field value is false, then mark ambiguous products. Remember that the [[discriminatorField]] annotation has precedence over the [[DiscriminatorDecider]].
		if (discriminatorOverride.fold(true)(!_.required)) {
			// sets the `isAmbiguous` flag to every product whose required field names match those of another product.
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

		val prefixInserterType = appliedType(typeOf[PrefixInserter[_, _]].typeConstructor, List(coproductType, typeOf[CoproductsOnly]));
		val prefixInserterInstance = ctx.inferImplicitValue(prefixInserterType, silent = true, withMacrosDisabled = true)

		val discriminatorDeciderType = appliedType(typeOf[DiscriminatorDecider[_, _]].typeConstructor, List(coproductType, typeOf[CoproductsOnly]));
		val discriminatorDeciderInstance = ctx.inferImplicitValue(discriminatorDeciderType, silent = true, withMacrosDisabled = true)

		val discriminatorValueMapperType = appliedType(typeOf[DiscriminatorValueMapper[_, _]].typeConstructor, List(coproductType, typeOf[CoproductsOnly]));
		val discriminatorValueMapperInstance = ctx.inferImplicitValue(discriminatorValueMapperType, silent = true, withMacrosDisabled = true)

		// for every product, generate the code lines that creates the [[CahProductInfo]] and adds it to the `productsInfoBuilder`
		val addProductInfo_codeLines =
			for {
				productInfo <- productsInfo
			} yield {
				val productClassNameAtRuntime = productInfo.tpe.erasure.typeSymbol.fullName;

				productInfo match {
					case customProductInfo: CustomProductInfo =>
						q"""productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, ${customProductInfo.appenderTree}));"""

					case derivedProductInfo: DerivedProductInfo =>

						val (aDiscriminatorExists: YesOrNo, appendDiscriminator_codeLine: Tree) = discriminatorOverride match {

							case Some(discriminatorAnnotation) => // If the initial coproduct is annotated with `@discriminatorField`
								if (productInfo.isAmbiguous || discriminatorAnnotation.required) {
									if (discriminatorValueMapperInstance == EmptyTree) {
										val discriminatorField = s""""${discriminatorAnnotation.fieldName}":"${derivedProductInfo.symbolName}"""";
										(Yes, q"r.append($discriminatorField)");
									} else {
										val discriminatorFieldHead = s""""${discriminatorAnnotation.fieldName}":"""";
										(Yes, q"""r.append($discriminatorFieldHead).append($discriminatorValueMapperInstance.apply(${derivedProductInfo.symbolName})).append('"')""")
									}
								} else {
									(No, EmptyTree);
								}

							case None => // If the initial coproduct is not annotated with `@discriminatorField`
								if (discriminatorValueMapperInstance == EmptyTree) { // if no `DiscriminatorValueMapper` is found in the implicit scope
									def discriminatorFieldValue = s"""${derivedProductInfo.symbolName}"""";
									if (productInfo.isAmbiguous) {
										(Yes, q"""r.append(discrimHead).append($discriminatorFieldValue)""")
									} else if (discriminatorDeciderInstance == EmptyTree) {
										(No, EmptyTree)
									} else {
										(Ask(TermName("discrimRequired")), q"""if (discrimRequired) { r.append(discrimHead).append($discriminatorFieldValue) }""")
									}

								} else { // if a `DiscriminatorValueMapper` is found in the implicit scope
									def discriminatorField = q"""r.append(discrimHead).append($discriminatorValueMapperInstance.apply(${derivedProductInfo.symbolName})).append('"');"""
									if (productInfo.isAmbiguous) {
										(Yes, discriminatorField)
									} else if (discriminatorDeciderInstance == EmptyTree) {
										(No, EmptyTree)
									} else {
										(Ask(TermName("discrimRequired")), q"""if (discrimRequired) $discriminatorField""")
									}
								}
						}

						val appendNonDiscriminatorFields_codeLines: Tree =
							if (prefixInserterInstance == EmptyTree) {
								if (derivedProductInfo.appendField_codeLines.isEmpty) {
									EmptyTree
								} else {
									aDiscriminatorExists match {
										case Yes => q"r.append(','); ..${derivedProductInfo.appendField_codeLines}"
										case No => q"..${derivedProductInfo.appendField_codeLines}"
										case Ask(tn) => q"if($tn) { r.append(',') }; ..${derivedProductInfo.appendField_codeLines}"
									}
								}
							} else {
								val getPrefix_code = q"val prefix = $prefixInserterInstance.fragment(p, true, ${derivedProductInfo.symbolName})"
								if (derivedProductInfo.appendField_codeLines.isEmpty) {
									aDiscriminatorExists match {
										case Yes => q"$getPrefix_code; if (prefix.nonEmpty) { r.append(',').append(prefix) };"
										case No => q"$getPrefix_code; r.append(prefix)"
										case Ask(tn) => q"$getPrefix_code; if ($tn && prefix.nonEmpty) { r.append(',') }; r.append(prefix);"
									}
								} else {
									aDiscriminatorExists match {
										case Yes => q"$getPrefix_code; if (prefix.nonEmpty) { r.append(',').append(prefix); }; r.append(','); ..${derivedProductInfo.appendField_codeLines}"
										case No => q"$getPrefix_code; if (prefix.nonEmpty) { r.append(prefix); r.append(',') }; ..${derivedProductInfo.appendField_codeLines}"
										case Ask(tn) => q"$getPrefix_code; if ($tn) { r.append(',') }; if (prefix.nonEmpty) { r.append(prefix).append(',') }; ..${derivedProductInfo.appendField_codeLines}"
									}
								}
							}

						q"""
val productAppender: _root_.jsfacile.write.Appender[${derivedProductInfo.tpe}] = { (r, p) =>
	r.append('{')

	$appendDiscriminator_codeLine

 	$appendNonDiscriminatorFields_codeLines

	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""

				}
			}
		val discriminatorDecider_valsCodeLines =
			if (discriminatorOverride.isEmpty) {
				if (discriminatorDeciderInstance == EmptyTree) {
					q"""
val discrimRequired = false;
val discrimHead = "\"?\":\""
"""
				} else {
					q"""
val discrimRequired = $discriminatorDeciderInstance.required;
val discrimHead = "\"" + $discriminatorDeciderInstance.fieldName + "\":\"";
"""
				}
			} else {
				q""
			}

		val createAppenderCodeLines =
			q"""
..${discriminatorDecider_valsCodeLines.children.take(3)}

val productsInfoBuilder = ArrayBuffer[CahProductInfo[_ <: $coproductType]]();

..$addProductInfo_codeLines

val productsArray = productsInfoBuilder.toArray.asInstanceOf[Array[CahProductInfo[$coproductType]]];
_root_.java.util.Arrays.sort(productsArray, productInfoComparator);
new CoproductAppender[$coproductType](${coproductType.toString}, productsArray)""";

		val createAppenderCodeLinesWithContext =
			q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.jsfacile.write.CoproductAppender;
import CoproductAppender.{CahProductInfo, productInfoComparator};
import _root_.jsfacile.macros.LazyAppender;

(appendersBuffer: Array[LazyAppender]) => $createAppenderCodeLines;""";

		coproductHandler.creationTreeOrErrorMsg = Some(Right(createAppenderCodeLines));

		//	ctx.info(ctx.enclosingPosition, s"coproduct appender builder for ${show(coproductType)} :\n${show(createAppenderCodeLines)}\n------${Handler.showAppenderDependencies(coproductHandler)}\n$showEnclosingMacros", force = false);

		// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[appenderHandlersMap]], and this macro execution needs to know of them later.
		expandNestedMacros(createAppenderCodeLinesWithContext);
		coproductHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
		//	ctx.info(ctx.enclosingPosition, s"coproduct appender after builder check for ${show(coproductType)}", force = false);
	}


	private def addSubtypesOf(
		coproductClassSymbol: ClassSymbol,
		coproductType: Type,
		initialHandler: Handler,
		productsInfoCollector: mutable.ArrayBuffer[ProductInfo]
	): Unit = {
		for (productSymbol <- coproductClassSymbol.knownDirectSubclasses.toIndexedSeq)
			this.addSubtype(productSymbol.asClass, coproductClassSymbol, coproductType, initialHandler, productsInfoCollector)
	}

	protected def addSubtype(
		subtypeClassSymbol: ClassSymbol,
		coproductClassSymbol: ClassSymbol,
		coproductType: Type,
		initialHandler: Handler, // may be mutated
		productsInfoCollector: mutable.ArrayBuffer[ProductInfo]
	): Unit = {
		this.applySubclassTypeConstructor(coproductType, subtypeClassSymbol.toTypeConstructor) match {
			case Right(subtypeType) =>
				if (subtypeType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and therefore not considered by the ambiguity detector below.

					if (subtypeClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields
						productsInfoCollector.addOne(DerivedProductInfo(
							subtypeClassSymbol.name.toString,
							subtypeClassSymbol.toType,
							Set.empty,
							Nil
						))

					} else if (subtypeClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addSubtypesOf` recursively to add all its concrete subtypes.
						if (subtypeClassSymbol.isSealed) {
							addSubtypesOf(subtypeClassSymbol, subtypeType, initialHandler, productsInfoCollector)
						} else {
							val msg = s"$subtypeClassSymbol should be sealed";
							initialHandler.setFailed(msg);
							ctx.abort(ctx.enclosingPosition, msg)
						}

					} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose fields are the parameters of said subclass primary constructor.
						val productCtorParamsLists = subtypeType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(subtypeType).dealias.paramLists;

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
								if (paramTypeSymbol != definitions.OptionClass) {
									requiredFieldNamesBuilder.addOne(paramNameStr)
								}

								val oGetAlreadyExpandedAppenderExpression =
									if (paramTypeSymbol.isClass) {
										Handler.appenderHandlersMap.get(new TypeKey(paramType)) match {

											case Some(paramHandler) =>
												initialHandler.addDependency(paramHandler);
												Some(q"""appendersBuffer(${paramHandler.typeIndex}).get[$paramType]""")

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

						productsInfoCollector.addOne(DerivedProductInfo(
							subtypeClassSymbol.name.toString,
							subtypeType,
							requiredFieldNamesBuilder.result(),
							appendField_codeLines
						))
					}

				}

			case Left(freeTypeParams) =>
				val msg = s"""The "$subtypeClassSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}.""";
				initialHandler.setFailed(msg)
				ctx.abort(ctx.enclosingPosition, msg)
		}
	}

}



