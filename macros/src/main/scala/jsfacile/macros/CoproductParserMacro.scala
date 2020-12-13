package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.annotations.discriminatorField
import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot

class CoproductParserMacro[Ctx <: blackbox.Context](val ctx: Ctx) {
	import ctx.universe._

	/** Macro implicit materializer of [[CoproductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl[C](coproductType: Type, coproductClassSymbol: ClassSymbol): ctx.Expr[Parser[C]] = {

		if (!coproductClassSymbol.isSealed) {
			ctx.abort(ctx.enclosingPosition, s"$coproductClassSymbol is not sealed")
		}

//		ctx.info(ctx.enclosingPosition, s"Coproduct parser helper start for ${show(coproductType)}", force = false)

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductHandler = parserHandlersMap.get(coproductTypeKey) match {
			case None =>
				val cpTypeIndex = parserHandlersMap.size;
				val coproductHandler = new Handler(cpTypeIndex);
				registerParserDependency(coproductHandler);
				parserHandlersMap.put(coproductTypeKey, coproductHandler);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorFieldName: Tree = {
					discriminatorField.parse(ctx.universe)(coproductClassSymbol) match {
						case Some((fieldName, _)) => q"$fieldName"

						case None => q"DiscriminatorDecider[$coproductType].fieldName"
					}
				};

				val productAdditionTrees: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty;
				val lastConsideredFieldBit = addProductsBelongingTo(coproductClassSymbol, coproductType, coproductType, coproductHandler, BitSet.FIRST_BIT_SLOT, mutable.Map.empty[String, ConsideredField], productAdditionTrees);

				val parserCreationTree =
					q"""
import _root_.scala.Array;

import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.macros.LazyParser;
import _root_.jsfacile.read.{Parser, CoproductParser, CoproductParserBuilderState};
import _root_.jsfacile.util.BitSet;
import CoproductParser.{CpProductInfo, CpFieldInfo, CpConsideredField};

val createParser: Array[LazyParser] => CoproductParser[$coproductType] = parsersBuffer => {
	val state = new CoproductParserBuilderState[$coproductType];

	..${productAdditionTrees}

	new CoproductParser[$coproductType](
		${coproductType.toString},
		$discriminatorFieldName,
		state.productsInfo,
		state.consideredFields,
		${lastConsideredFieldBit.shardIndex + 1}
	)
};
createParser""";

				coproductHandler.creationTreeOrErrorMsg = Some(Right(parserCreationTree));

				ctx.info(ctx.enclosingPosition, s"coproduct parser unchecked builder for ${show(coproductType)} : ${show(parserCreationTree)}\n------${showParserDependencies(coproductHandler)}\n${showEnclosingMacros(ctx)}", force = false);
				// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[parserHandlersMap]], and this macro execution needs to know of them later.
				ctx.typecheck(parserCreationTree/*.duplicate*/); // the duplicate is necessary because, according to Dymitro Mitin, the `typeCheck` method mutates its argument sometimes.
				coproductHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				ctx.info(ctx.enclosingPosition, s"coproduct parser after builder check for ${show(coproductType)}", force = false);

				coproductHandler

			case Some(coproductHandler) =>
				registerParserDependency(coproductHandler);
				coproductHandler
		}

		val body = CustomParserMacro.buildParserBody[ctx.type](ctx)(coproductType, coproductHandler)

		ctx.info(ctx.enclosingPosition, s"coproduct parser body for ${show(coproductType)}: ${show(body)}\n------${showParserDependencies(coproductHandler)}\n${showEnclosingMacros(ctx)}", force = false);

		ctx.Expr[Parser[C]](body);
	}


	/** Adds to the `productsSnippetBuilder` a snippet for each product that extends the specified trait (or abstract class). */
	private def addProductsBelongingTo(
		coproductClassSymbol: ClassSymbol, // the starting coproduct symbol or, in deeper levels of recursion, an abstract extension of it
		coproductType: Type, // the starting coproduct type or, in deeper levels of recursion, an abstract extension of it
		rootType: Type, // the starting coproduct type
		rootKeeper: Keeper, // the keeper/handler of the starting coproduct. This parameter may be mutated
		startingBitSlot: BitSlot, // The bit slot that will be assigned to the next field that be added to the considered fields set.
		metaConsideredFields: mutable.Map[String, ConsideredField], // a map that memorizes info about the the fields for with the [[Tree]] that adds an instance of [[CpConsideredField]] to the `consideredFieldsBuilder` was already generated.
		productAdditionTrees: mutable.ArrayBuffer[Tree] // the buffer where the [[Tree]]s that add a [[CpProductInfo]] instance to the [[CoproductParserBuilderState]] are collected.
	): BitSlot = {
		var nextBitSlot = startingBitSlot;
		for {
			productSymbol <- coproductClassSymbol.knownDirectSubclasses
		} {
			val productClassSymbol = productSymbol.asClass;
			nextBitSlot = this.addProduct(productClassSymbol, coproductClassSymbol, coproductType, rootType, rootKeeper, nextBitSlot, metaConsideredFields, productAdditionTrees)
		}
		nextBitSlot
	}


	def addProduct(
		productClassSymbol: ClassSymbol, // the class symbol of the product to add
		coproductClassSymbol: ClassSymbol, // the starting coproduct symbol or, in deeper levels of recursion, an abstract extension of it
		coproductType: Type, // the starting coproduct type or, in deeper levels of recursion, an abstract extension of it
		rootType: Type, // the starting coproduct type
		rootKeeper: Keeper, // the keeper/handler of the starting coproduct. This parameter may be mutated
		startingBitSlot: BitSlot, // The bit slot that will be assigned to the next field that be added to the considered fields set.
		metaConsideredFields: mutable.Map[String, ConsideredField], // a map that memorizes info about the the fields for with the [[Tree]] that adds an instance of [[CpConsideredField]] to the [[CoproductParserBuilderState]] was already generated.
		productAdditionTrees: mutable.ArrayBuffer[Tree] // the buffer where the [[Tree]]s that add a [[CpProductInfo]] instance to the [[CoproductParserBuilderState]] are collected.
	): BitSlot = {
		var nextBitSlot = startingBitSlot;

		ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productClassSymbol.toTypeConstructor) match {
			case Right(productType) =>
				if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This is and edge case that occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and, therefore, not added to the products info set.

					if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields nor constructor.
						productAdditionTrees.addOne(
							q"""
state.addProduct(CpProductInfo[$coproductType](
	${productClassSymbol.name.toString},
	BitSet.empty(),
	0,
	Array.empty[CpFieldInfo],
	(args: Seq[Any]) => ${productClassSymbol.module}
));"""
						)

					} else if (productClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
						if (productClassSymbol.isSealed) {
							nextBitSlot = addProductsBelongingTo(productClassSymbol, productType, rootType, rootKeeper, nextBitSlot, metaConsideredFields, productAdditionTrees)
						} else {
							val msg = s"$productClassSymbol should be sealed"
							rootKeeper.setFailed(msg);
							ctx.abort(ctx.enclosingPosition, s"$productClassSymbol should be sealed")
						}

					} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose constructor is its primary one, and its fields are its primary constructor parameters.
						val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).paramLists

						val addProductFieldSnippetSeqBuilder = Seq.newBuilder[ctx.Tree];
						var requiredFieldsAccum: BitSet = BitSet.empty(nextBitSlot.shardIndex + 1);
						var requiredFieldsCounter: Int = 0;
						var argIndex = -1;
						val ctorArgumentsTrees =
							for (params <- productCtorParamsLists) yield {
								for (param <- params) yield {
									val paramType = param.typeSignature.dealias
									val paramTypeSymbol = paramType.typeSymbol;
									argIndex += 1;

									val (isRequired, defaultValue_expression) =
										if (paramTypeSymbol.fullName == "scala.Option") {
											(false, q"Some(None)")

										} else { // TODO add support of scala parameter default value
											requiredFieldsCounter += 1;
											(true, q"None")
										}

									val fieldName = param.name.toString;
									val addProductField_snippet =
										metaConsideredFields.get(fieldName) match {

											case Some(ConsideredField(fieldType, firstOwnerName, consideredFieldIndex, consideredFieldBitSlot)) =>
												if (isRequired) {
													requiredFieldsAccum = requiredFieldsAccum.add(consideredFieldBitSlot);
												}

												if (paramType =:= fieldType.asInstanceOf[ctx.Type]) {
													q"""state.addProductField(CpFieldInfo($fieldName, $consideredFieldIndex, $argIndex, $defaultValue_expression));"""
												} else {
													val msg = s"""Unsupported situation while building a `Parser[$rootType]`: two implementations, `$productClassSymbol` and `$firstOwnerName`, have a field with the same name ("$fieldName") but different type."""
													rootKeeper.setFailed(msg);
													ctx.abort(ctx.enclosingPosition, msg)
												}

											case None =>
												val consideredFieldBitSlot = nextBitSlot;
												nextBitSlot = nextBitSlot.shifted;

												if (isRequired) {
													requiredFieldsAccum = requiredFieldsAccum.add(consideredFieldBitSlot);
												}

												val consideredFieldIndex = metaConsideredFields.size;
												metaConsideredFields.put(fieldName, ConsideredField(paramType, productClassSymbol.fullName, consideredFieldIndex, consideredFieldBitSlot))

												val paramParserExpression: ctx.Tree =
													if (paramTypeSymbol.isClass) {
														parserHandlersMap.get(new TypeKey(paramType)) match {
															case Some(paramHandler) =>
																rootKeeper.addDependency(paramHandler);

																if (!paramTypeSymbol.isAbstract || paramTypeSymbol.asClass.isSealed) {
																	q"""parsersBuffer(${paramHandler.typeIndex}).get"""
																} else {
																	val msg = s"Unreachable reached: fieldName = $fieldName, productType = $productType, paramTypeSymbol = ${paramTypeSymbol.fullName}\n------${showParserDependencies(paramHandler)}\n${showEnclosingMacros(ctx)}"
																	rootKeeper.setFailed(msg);
																	ctx.abort(ctx.enclosingPosition, msg)
																}
															case None =>
																q"Parser[$paramType]"
														}
													} else {
														q"Parser[$paramType]"
													}



												q"""
state.addProductField(CpFieldInfo($fieldName, $consideredFieldIndex, $argIndex, $defaultValue_expression));
state.addConsideredField(CpConsideredField($fieldName, $consideredFieldIndex, BitSet.BitSlot(${consideredFieldBitSlot.shardMask}, ${consideredFieldBitSlot.shardIndex}), $paramParserExpression));"""
										}
									addProductFieldSnippetSeqBuilder.addOne(addProductField_snippet);

									q"args($argIndex).asInstanceOf[$paramType]";
								}
							}
						val ctorFunction = q"(args: Seq[Any]) => new $productClassSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

						productAdditionTrees.addOne(
							q"""
..${addProductFieldSnippetSeqBuilder.result()}
state.addProduct(CpProductInfo[$coproductType](
	${productClassSymbol.name.toString},
	new BitSet(Array(..${new ArraySeq.ofLong(requiredFieldsAccum.shards)})),
	$requiredFieldsCounter,
	state.productFields,
	$ctorFunction
));
state.productFieldsBuilder.clear();"""
						)
					}
				}

			case Left(freeTypeParams) =>
				val msg =s"""The "$productClassSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}."""
				rootKeeper.setFailed(msg);
				ctx.abort(ctx.enclosingPosition, msg)
		}
		nextBitSlot
	}
}



