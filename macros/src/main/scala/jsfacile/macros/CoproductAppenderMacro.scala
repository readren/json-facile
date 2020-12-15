package jsfacile.macros

import scala.collection.mutable
import scala.collection.IndexedSeq
import scala.reflect.macros.blackbox

import jsfacile.annotations.discriminatorField
import jsfacile.joint.DiscriminatorConf
import jsfacile.write.Appender

class CoproductAppenderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends AppenderGenCommon(context) {
	import ctx.universe._

	protected case class ProductInfo(simpleName: String, tpe: Type, requiredFieldNames: Set[String], appendField_codeLines: List[Tree]) {
		var isAmbiguous = false;
	}

	def materializeImpl(initialCoproductType: Type, initialCoproductClassSymbol: ClassSymbol): ctx.Expr[Appender[C]] = {

		if (!initialCoproductClassSymbol.isSealed) {
			ctx.abort(ctx.enclosingPosition, s"`$initialCoproductClassSymbol` is not sealed.")
		}

		//	ctx.info(ctx.enclosingPosition, s"coproduct appender start for ${show(coproductType)}", force = false);

		val coproductTypeKey = new TypeKey(initialCoproductType);
		val coproductHandler = appenderHandlersMap.get(coproductTypeKey) match {

			case None =>
				val caTypeIndex = appenderHandlersMap.size;
				val coproductHandler = new Handler(caTypeIndex)
				registerAppenderDependency(coproductHandler);
				appenderHandlersMap.put(coproductTypeKey, coproductHandler);

				val productsInfoCollector: mutable.ArrayBuffer[ProductInfo] = mutable.ArrayBuffer.empty;
				addProductsBelongingTo(initialCoproductClassSymbol, initialCoproductType, coproductHandler, productsInfoCollector);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorOverride = discriminatorField.parse(ctx.universe)(initialCoproductClassSymbol);

				buildAppenderCreationTreeOn(initialCoproductType, coproductHandler, discriminatorOverride, productsInfoCollector);

				coproductHandler

			case Some(coproductHandler) =>
				registerAppenderDependency(coproductHandler)
				coproductHandler

		};

		this.buildBody[C](initialCoproductType, coproductHandler);
	}


	def buildAppenderCreationTreeOn(coproductType: Type, coproductHandler: Handler, discriminatorOverride: Option[DiscriminatorConf], productsInfo: IndexedSeq[ProductInfo]): Unit = {

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

		// for every product, generate the code lines that creates the [[CahProductInfo]] and adds it to the `productsInfoBuilder`
		val addProductInfo_codeLines =
			for {
				productInfo <- productsInfo
			} yield {
				val discriminatorFieldValue =
					if (productInfo.appendField_codeLines.isEmpty) s"""":"${productInfo.simpleName}"""";
					else s"""":"${productInfo.simpleName}",""";

				val appendDiscriminator_codeLine = discriminatorOverride match {
					case Some(discriminatorAnnotation) =>
						if (productInfo.isAmbiguous || discriminatorAnnotation.required) {
							val discriminatorField = s""""${discriminatorAnnotation.fieldName}$discriminatorFieldValue""";
							q"r.append($discriminatorField)";
						} else {
							q"";
						}

					case None =>
						if (productInfo.isAmbiguous) {
							q"""r.append(discrimPrefix).append($discriminatorFieldValue)"""
						} else {
							q"""if (discrimRequired) { r.append(discrimPrefix).append($discriminatorFieldValue) }"""
						}
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
		val discriminatorDecider_valsCodeLines =
			if (discriminatorOverride.isEmpty) {
				q"""
val discrimDecider = DiscriminatorDecider[$coproductType];
val discrimRequired = discrimDecider.required;
val discrimPrefix = "\"" + discrimDecider.fieldName;
"""
			} else {
				q""
			}


		val createAppenderCodeLines =
			q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.write.CoproductAppender;
import CoproductAppender.{CahProductInfo, productInfoComparator};
import _root_.jsfacile.macros.LazyAppender;

val createAppender: Array[LazyAppender] => CoproductAppender[$coproductType] = appendersBuffer => {

	..${discriminatorDecider_valsCodeLines.children.take(3)}

	val productsInfoBuilder = ArrayBuffer[CahProductInfo[_ <: $coproductType]]();

	..$addProductInfo_codeLines

	val productsArray = productsInfoBuilder.toArray.asInstanceOf[Array[CahProductInfo[$coproductType]]];
	_root_.java.util.Arrays.sort(productsArray, productInfoComparator);
	new CoproductAppender[$coproductType](${coproductType.toString}, productsArray)
};
createAppender""";

		coproductHandler.creationTreeOrErrorMsg = Some(Right(createAppenderCodeLines));

		ctx.info(ctx.enclosingPosition, s"coproduct appender unchecked builder for ${show(coproductType)} : ${show(createAppenderCodeLines)}\n------${showAppenderDependencies(coproductHandler)}\n$showEnclosingMacros", force = false);
		// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[appenderHandlersMap]], and this macro execution needs to know of them later.
		ctx.typecheck(createAppenderCodeLines);
		coproductHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
		ctx.info(ctx.enclosingPosition, s"coproduct appender after builder check for ${show(coproductType)}", force = false);
	}


	private def addProductsBelongingTo(
		coproductClassSymbol: ClassSymbol,
		coproductType: Type,
		initialHandler: Handler,
		productsInfoCollector: mutable.ArrayBuffer[ProductInfo]
	): Unit = {
		for(productSymbol <- coproductClassSymbol.knownDirectSubclasses.toIndexedSeq)
			this.addProduct(productSymbol.asClass, coproductClassSymbol, coproductType, initialHandler, productsInfoCollector)
	}

	def addProduct(
		productClassSymbol: ClassSymbol,
		coproductClassSymbol: ClassSymbol,
		coproductType: Type,
		initialHandler: Handler,
		productsInfoCollector: mutable.ArrayBuffer[ProductInfo]
	): Unit = {
		this.applySubclassTypeConstructor(coproductType, productClassSymbol.toTypeConstructor) match {
			case Right(productType) =>
				if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and therefore not considered by the ambiguity detector below.

					if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields
						productsInfoCollector.addOne(ProductInfo(
							productClassSymbol.name.toString,
							productClassSymbol.toType,
							Set.empty,
							Nil
						))

					} else if (productClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
						if (productClassSymbol.isSealed) {
							addProductsBelongingTo(productClassSymbol, productType, initialHandler, productsInfoCollector)
						} else {
							val msg = s"$productClassSymbol should be sealed";
							initialHandler.setFailed(msg);
							ctx.abort(ctx.enclosingPosition, msg)
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
												initialHandler.addDependency(paramHandler);

												if (!paramTypeSymbol.isAbstract || paramTypeSymbol.asClass.isSealed) {
													Some(q"""appendersBuffer(${paramHandler.typeIndex}).get[$paramType]""")
												} else {
													val msg = s"Unreachable reached: productType=$productType, paramTypeSymbol=${paramTypeSymbol.fullName}"
													initialHandler.setFailed(msg);
													ctx.abort(ctx.enclosingPosition, msg)
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

						productsInfoCollector.addOne(ProductInfo(
							productClassSymbol.name.toString,
							productType,
							requiredFieldNamesBuilder.result(),
							appendField_codeLines
						))
					}

				}

			case Left(freeTypeParams) =>
				val msg = s"""The "$productClassSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}.""";
				initialHandler.setFailed(msg)
				ctx.abort(ctx.enclosingPosition, msg)
		}
	}

}



