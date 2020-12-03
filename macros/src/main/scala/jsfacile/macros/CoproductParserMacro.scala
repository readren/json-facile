package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.joint.{CoproductUpperBound, discriminatorField}
import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot

object CoproductParserMacro {


	/** Macro implicit materializer of [[CoproductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl[C <: CoproductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Parser[C]] = {
		import ctx.universe._

		/** @param fieldType the type of the field
		 * @param firstOwner the TypeSymbol of the first product that contains a field named as the key specifies.
		 * @param consideredFieldIndex the index of the field in the `consideredFields` parameter of the [[jsfacile.read.CoproductParser]] constructor.
		 * @param bitSlot the [[BitSlot]] assigned to this considered field*/
		case class ConsideredField(fieldType: Type, firstOwner: ClassSymbol, consideredFieldIndex: Int, bitSlot: BitSlot)

		/** Adds to the `productsSnippetBuilder` a snippet for each product that extends the specified trait (or abstract class). */
		def addProductsBelongingTo(
			coproductClassSymbol: ClassSymbol, // the starting coproduct symbol or, in deeper levels of recursion, an abstract extension of it
			coproductType: Type, // the starting coproduct type or, in deeper levels of recursion, an abstract extension of it
			rootType: Type, // the starting coproduct type
			rootHandler: Handler, // the handler of the starting coproduct. This parameter may be mutated
			startingBitSlot: BitSlot, // The bit slot that will be assigned to the next field that be added to the considered fields set.
			consideredFields: mutable.Map[String, ConsideredField], // a map that memorizes info about the the fields that were already added to the `consideredFieldsBuilder`.
			productsSnippetsBuilder: mutable.Builder[Tree, Seq[Tree]] // the Seq builder where the code snippets are collected.
		): BitSlot = {
			var nextBitSlot = startingBitSlot;
			for {
				productSymbol <- coproductClassSymbol.knownDirectSubclasses
			} {
				val productClassSymbol = productSymbol.asClass;
				ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productClassSymbol.toTypeConstructor) match {
					case Right(productType) =>
						if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This is and edge case that occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and, therefore, not added to the products info set.

							if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields nor constructor.
								productsSnippetsBuilder.addOne(
									q"""
productsInfoBuilder.addOne(CpProductInfo(
	${productClassSymbol.name.toString},
	BitSet.empty(),
	0,
	Array.empty[CpFieldInfo],
	(args: Seq[Any]) => ${productClassSymbol.module}
));"""
								)

							} else if (productClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
								if (productClassSymbol.isSealed) {
									nextBitSlot = addProductsBelongingTo(productClassSymbol, productType, rootType, rootHandler, nextBitSlot, consideredFields, productsSnippetsBuilder)
								} else {
									ctx.abort(ctx.enclosingPosition, s"$productClassSymbol should be sealed")
								}

							} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose constructor is its primary one, and its fields are its primary constructor parameters.
								val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).paramLists

								val addProductFieldSnippetSeqBuilder = Seq.newBuilder[ctx.Tree];
								var requiredFieldsAccum: BitSet = BitSet.empty(startingBitSlot.shardIndex + 1);
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
												consideredFields.get(fieldName) match {
													case Some(ConsideredField(fieldType, firstOwner, consideredFieldIndex, consideredFieldBitSlot)) =>

														if (isRequired) {
															requiredFieldsAccum = requiredFieldsAccum.add(consideredFieldBitSlot);
														}

														if (fieldType =:= paramType) {
															q"""productFieldsBuilder.addOne(CpFieldInfo($fieldName, $consideredFieldIndex, $argIndex, $defaultValue_expression));"""
														} else {
															ctx.abort(ctx.enclosingPosition, s"""Unsupported situation While building a `Parser[$rootType]`: two implementations, $productSymbol and $firstOwner, have a field with the same name ("$fieldName") but different type.""")
														}

													case None =>

														val consideredFieldBitSlot = nextBitSlot;
														nextBitSlot = nextBitSlot.shifted;

														if (isRequired) {
															requiredFieldsAccum = requiredFieldsAccum.add(consideredFieldBitSlot);
														}

														val consideredFieldIndex = consideredFields.size;
														consideredFields.put(fieldName, ConsideredField(paramType, productClassSymbol, consideredFieldIndex, consideredFieldBitSlot))

														val paramParserExpression: ctx.Tree =
															if (paramTypeSymbol.isClass) {
																parserHandlersMap.get(new TypeKey(paramType)) match {
																	case Some(paramHandler) =>
																		rootHandler.addDependency(paramHandler);

																		if (!paramTypeSymbol.isAbstract || paramTypeSymbol.asClass.isSealed) {
																			q"""parsersBuffer(${paramHandler.typeIndex}).get"""
																		} else {
																			ctx.abort(ctx.enclosingPosition, "Unreachable")
																		}
																	case None =>
																		q"Parser[$paramType]"
																}
															} else {
																q"Parser[$paramType]"
															}



														q"""
productFieldsBuilder.addOne(CpFieldInfo($fieldName, $consideredFieldIndex, $argIndex, $defaultValue_expression));
consideredFieldsBuilder.addOne(CpConsideredField($fieldName, $consideredFieldIndex, BitSet.BitSlot(${consideredFieldBitSlot.shardMask}, ${consideredFieldBitSlot.shardIndex}), $paramParserExpression));"""
												}
											addProductFieldSnippetSeqBuilder.addOne(addProductField_snippet);

											q"args($argIndex).asInstanceOf[$paramType]";
										}
									}
								val ctorFunction = q"(args: Seq[Any]) => new $productClassSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

								productsSnippetsBuilder.addOne(
									q"""
..${addProductFieldSnippetSeqBuilder.result()}
val productFieldsArray = productFieldsBuilder.sortInPlace()(namedOrdering[CpFieldInfo]).toArray;
productsInfoBuilder.addOne(CpProductInfo(
	${productClassSymbol.name.toString},
	new BitSet(Array(..${new ArraySeq.ofLong(requiredFieldsAccum.shards)})),
	$requiredFieldsCounter,
	productFieldsArray,
	$ctorFunction
));
productFieldsBuilder.clear();"""
								)
							}
						}

					case Left(freeTypeParams) =>
						ctx.abort(ctx.enclosingPosition, s"""The "$productSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}.""")
				}
			}
			nextBitSlot
		}

		//// the body of this method starts here ////

		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (!coproductSymbol.isClass || !coproductSymbol.isAbstract) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a trait or abstract class")
		}
		val coproductClassSymbol = coproductSymbol.asClass;
		if (!coproductClassSymbol.isSealed) {
			ctx.abort(ctx.enclosingPosition, s"$coproductClassSymbol is not sealed")
		}

		ctx.echo(ctx.enclosingPosition, s"Coproduct parser helper start for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}")

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

				val productsSnippetsBuilder = Seq.newBuilder[ctx.universe.Tree];
				val lastConsideredFieldBit = addProductsBelongingTo(coproductClassSymbol, coproductType, coproductType, coproductHandler, BitSet.FIRST_BIT_SLOT, mutable.Map.empty[String, ConsideredField], productsSnippetsBuilder);

				val createParserCodeLines =
					q"""
import _root_.scala.collection.immutable.ArraySeq;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.scala.math.Ordering;

import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.macros.{LazyParser, namedOrdering};
import _root_.jsfacile.read.{Parser, CoproductParser};
import _root_.jsfacile.util.BitSet;
import CoproductParser.{CpProductInfo, CpFieldInfo, CpConsideredField};

val createParser: Array[LazyParser] => CoproductParser[$coproductType] = parsersBuffer => {
	val productsInfoBuilder = ArrayBuffer[CpProductInfo[_ <: $coproductType]]();
	val consideredFieldsBuilder = ArrayBuffer[CpConsideredField]();
	val productFieldsBuilder = ArrayBuffer[CpFieldInfo]();

	..${productsSnippetsBuilder.result()}

	new CoproductParser[$coproductType](
		${coproductType.toString},
		$discriminatorFieldName,
		productsInfoBuilder.sortInPlace()(namedOrdering[CpProductInfo[$coproductType]]).toArray,
		consideredFieldsBuilder.sortInPlace()(namedOrdering[CpConsideredField]).toArray,
		${lastConsideredFieldBit.shardIndex + 1}
	)
};
createParser""";

				coproductHandler.oExpression = Some(createParserCodeLines);

				ctx.echo(ctx.enclosingPosition, s"coproduct parser unchecked init for ${show(coproductType)} : ${show(createParserCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				ctx.typecheck(createParserCodeLines.duplicate); // the duplicate is necessary because, according to Dymitro Mitin, the `typeCheck` method mutates its argument sometimes.
				coproductHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				ctx.echo(ctx.enclosingPosition, s"coproduct parser after init check for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

				coproductHandler

			case Some(coproductHandler) =>
				registerParserDependency(coproductHandler);
				coproductHandler
		}

		val body =
			if (coproductHandler.oExpression.isDefined && isOuterParserMacroInvocation(ctx)) {
				val inits =
					for {
						(_, handler) <- parserHandlersMap
						if coproductHandler.doesDependOn(handler.typeIndex)
					} yield {
						val createParserCodeLines = handler.oExpression.get.asInstanceOf[ctx.Tree];
						q"""parsersBuffer(${handler.typeIndex}).set($createParserCodeLines(parsersBuffer));"""
					}

				q"""
import _root_.jsfacile.macros.LazyParser;

val parsersBuffer = _root_.scala.Array.fill(${parserHandlersMap.size})(new LazyParser);
{..$inits}
parsersBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""

			} else {
				q"""parsersBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"coproduct parser body for ${show(coproductType)}: ${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		ctx.Expr[Parser[C]](body);
	}
}



