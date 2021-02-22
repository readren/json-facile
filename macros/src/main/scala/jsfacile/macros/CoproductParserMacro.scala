package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.annotations.discriminatorField
import jsfacile.macros.GenCommon.TypeKey
import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot


/** @tparam C the type of the sealed abstract data type for which this macro materializes a [[jsfacile.read.Parser]]. */
class CoproductParserMacro[C, Ctx <: blackbox.Context](context: Ctx) extends ParserGenCommon(context) {
	import ctx.universe._

	/**Note: instances of this class exists only during compilation time.
	 *
	 * @param fieldType the type of the field
	 * @param firstOwnerName the TypeSymbol of the first product that contains a field named as the key specifies.
	 * @param consideredFieldIndex the index of the field in the `consideredFields` parameter of the [[jsfacile.read.CoproductParser]] constructor.
	 * @param bitSlot the [[jsfacile.util.BitSet.BitSlot]] assigned to this considered field*/
	case class ConsideredField(fieldType: Type, firstOwnerName: String, consideredFieldIndex: Int, bitSlot: BitSlot)

	/** Macro implicit materializer of [[CoproductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl(initialCoproductType: Type, initialCoproductClassSymbol: ClassSymbol): ctx.Expr[Parser[C]] = {

//		ctx.info(ctx.enclosingPosition, s"Coproduct parser helper start for ${show(initialCoproductType)}", force = false)

		val isOuterMacroInvocation = isOuterParserMacroInvocation;
		if(isOuterMacroInvocation) {
			/** Discard the [[Parser]]s generated in other code contexts. This is necessary because: (1) since the existence of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] the derived [[Parser]]s depends on the context; and (2) the existence of an [[Parser]] in the implicit scope depends on the context. */
			Handler.parserHandlersMap.clear();
		}

		val coproductTypeKey = new TypeKey(initialCoproductType);
		val coproductHandler = Handler.parserHandlersMap.get(coproductTypeKey) match {
			case None =>
				val cpTypeIndex = Handler.parserHandlersMap.size;
				val coproductHandler = new Handler(cpTypeIndex);
				Handler.registerParserDependency(coproductHandler);
				Handler.parserHandlersMap.put(coproductTypeKey, coproductHandler);

				if (!initialCoproductClassSymbol.isSealed) {
					val errorMsg = s"`$initialCoproductClassSymbol` is not sealed. Automatic derivation requires that abstract types be sealed. Seal it or use the `CoproductTranslatorsBuilder`.";
					coproductHandler.setFailed(errorMsg)
					ctx.abort(ctx.enclosingPosition, errorMsg)
				}

				val productAdditionTrees: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty;
				val lastConsideredFieldBit = addSubtypesOf(initialCoproductClassSymbol, initialCoproductType, initialCoproductType, coproductHandler, BitSet.FIRST_BIT_SLOT, mutable.Map.empty, productAdditionTrees);

				buildParserCreationTreeOn(coproductHandler, initialCoproductType, initialCoproductClassSymbol, productAdditionTrees, lastConsideredFieldBit.shardIndex + 1)
				coproductHandler

			case Some(coproductHandler) =>
				Handler.registerParserDependency(coproductHandler);
				coproductHandler
		}

		this.buildBody[C](initialCoproductType, coproductHandler, isOuterMacroInvocation)
	}

	protected def buildParserCreationTreeOn(coproductHandler: Handler, initialCoproductType: Type, initialCoproductClassSymbol: ClassSymbol, productAdditionTrees: Iterable[Tree], numberOfShards: Int): Unit = {

		// Get the discriminator field name and requirement from the coproduct annotation, or from the `DiscriminatorDecider` in the implicit scope if it isn't annotated.
		val discriminatorFieldName: Tree = {
			discriminatorField.parse(ctx)(initialCoproductClassSymbol) match {
				case Some(discriminatorConf) => Literal(Constant(discriminatorConf.fieldName))

				case None => q"DiscriminatorDecider[$initialCoproductType].fieldName"
			}
		};

		val parserCreationTree =
			q"""
	val state = new CoproductParserBuilderState[$initialCoproductType];

	..$productAdditionTrees

	new CoproductParser[$initialCoproductType](
		${initialCoproductType.toString},
		$discriminatorFieldName,
		state.productsInfo,
		state.consideredFields,
		$numberOfShards
	);""";

		val parserCreationTreeWithContext =
			q"""
import _root_.scala.Array;

import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.macros.LazyParser;
import _root_.jsfacile.read.{Parser, CoproductParser, CoproductParserBuilderState};
import _root_.jsfacile.util.BitSet;
import CoproductParser.{CpProductInfo, CpFieldInfo, CpConsideredField};

(parsersBuffer: Array[LazyParser]) => $parserCreationTree""";

		coproductHandler.creationTreeOrErrorMsg = Some(Right(parserCreationTree));

		ctx.info(ctx.enclosingPosition, s"coproduct parser unchecked builder for ${show(initialCoproductType)} :\n${show(parserCreationTree)}\n------${Handler.showParserDependencies(coproductHandler)}\n$showEnclosingMacros", force = false);
		// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[parserHandlersMap]], and this macro execution needs to know of them later.
		expandNestedMacros(parserCreationTreeWithContext);
		coproductHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
//		ctx.info(ctx.enclosingPosition, s"coproduct parser after builder check for ${show(initialCoproductType)}", force = false);
	}


	/** Adds to the `productAdditionTrees` a snippet for each concrete type that extends the specified coproduct type. */
	private def addSubtypesOf(
		coproductClassSymbol: ClassSymbol, // the starting coproduct symbol or, in deeper levels of recursion, an abstract extension of it
		coproductType: Type, // the starting coproduct type or, in deeper levels of recursion, an abstract extension of it
		initialCoproductType: Type, // the coproduct type received by this macro execution
		initialHandler: Handler, // the handler of the initial coproduct. This parameter may be mutated
		startingBitSlot: BitSlot, // The bit slot that this method will assign to the first field added to the considered fields set.
		metaConsideredFields: mutable.Map[String, ConsideredField], // the map where the info about the the fields, for with the [[Tree]] that adds an instance of [[CpConsideredField]] to the `consideredFieldsBuilder` was already generated, is collected
		productAdditionTrees: mutable.ArrayBuffer[Tree] // the buffer where the [[Tree]]s, that add a [[CpProductInfo]] instance to the [[CoproductParserBuilderState]], are collected.
	): BitSlot = {
		var nextBitSlot = startingBitSlot;
		for (productSymbol <- coproductClassSymbol.knownDirectSubclasses) {
			nextBitSlot = this.addSubtype(productSymbol.asClass, coproductClassSymbol, coproductType, initialCoproductType, initialHandler, nextBitSlot, metaConsideredFields, productAdditionTrees)
		}
		nextBitSlot
	}


	protected def addSubtype(
		subtypeClassSymbol: ClassSymbol, // the class symbol of the subtype to add
		coproductClassSymbol: ClassSymbol, // the starting coproduct symbol or, in deeper levels of recursion, an abstract extension of it
		coproductType: Type, // the starting coproduct type or, in deeper levels of recursion, an abstract extension of it
		initialCoproductType: Type, // the coproduct type received by this macro execution
		initialHandler: Handler, // the handler of the initial coproduct. This parameter may be mutated
		startingBitSlot: BitSlot, // The bit slot that this method will assign to the first field added to the considered fields set.
		metaConsideredFields: mutable.Map[String, ConsideredField], // the map where the info about the the fields, for with the [[Tree]] that adds an instance of [[CpConsideredField]] to the `consideredFieldsBuilder` was already generated, is collected
		productAdditionTrees: mutable.ArrayBuffer[Tree] // the buffer where the [[Tree]]s that add a [[CpProductInfo]] instance to the [[CoproductParserBuilderState]] are collected.
	): BitSlot = {
		var nextBitSlot = startingBitSlot;

		this.applySubclassTypeConstructor(coproductType, subtypeClassSymbol.toTypeConstructor) match {
			case Right(productType) =>
				if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This is and edge case that occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and, therefore, not added to the products info set.

					if (subtypeClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields nor constructor.
						productAdditionTrees.addOne(
							q"""
state.addProduct(CpProductInfo[$coproductType](
	${subtypeClassSymbol.name.toString},
	BitSet.empty(),
	0,
	Array.empty[CpFieldInfo],
	(args: Seq[Any]) => ${subtypeClassSymbol.module}
));"""
						)

					} else if (subtypeClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
						if (subtypeClassSymbol.isSealed) {
							nextBitSlot = addSubtypesOf(subtypeClassSymbol, productType, initialCoproductType, initialHandler, nextBitSlot, metaConsideredFields, productAdditionTrees)
						} else {
							val msg = s"$subtypeClassSymbol should be sealed"
							initialHandler.setFailed(msg);
							ctx.abort(ctx.enclosingPosition, s"$subtypeClassSymbol should be sealed")
						}

					} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose constructor is its primary one, and its fields are its primary constructor parameters.
						val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).paramLists

						val ics = new InterAddFieldCallsState(nextBitSlot);
						val ctorArgumentsTrees =
							for (params <- productCtorParamsLists) yield {
								for (param <- params) yield {
									val fieldType = param.typeSignature.dealias;
									val oDefaultValue = None; // TODO obtain it from the scala parameter default value
									addField(productType, subtypeClassSymbol, param.name.toString, param.typeSignature.dealias, oDefaultValue, initialCoproductType, initialHandler, metaConsideredFields, ics);

									q"args(${ics.argIndex}).asInstanceOf[$fieldType]";
								}
							}
						val ctorFunction = q"(args: Seq[Any]) => new $subtypeClassSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"
						nextBitSlot = ics.nextBitSlot;
						productAdditionTrees.addOne(
							q"""
..${ics.addFieldTreeSeqBuilder.result()}
state.addProduct(CpProductInfo[$coproductType](
	${subtypeClassSymbol.name.toString},
	new BitSet(Array(..${new ArraySeq.ofLong(ics.requiredFieldsAccum.shards)})),
	${ics.requiredFieldsCounter},
	state.productFields,
	$ctorFunction
));
state.productFieldsBuilder.clear();"""
						)
					}
				}

			case Left(freeTypeParams) =>
				val msg =s"""The "$subtypeClassSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}."""
				initialHandler.setFailed(msg);
				ctx.abort(ctx.enclosingPosition, msg)
		}
		nextBitSlot
	}

	/** State shared between calls to the [[addField]] method. */
	protected class InterAddFieldCallsState(var nextBitSlot: BitSlot) {
		/** the [[scala.Seq]] builder where the "add field" code snippets are collected */
		val addFieldTreeSeqBuilder: mutable.Builder[ctx.Tree, Seq[ctx.Tree]] = Seq.newBuilder[ctx.Tree];
		var requiredFieldsAccum: BitSet = BitSet.empty(nextBitSlot.shardIndex + 1);
		var requiredFieldsCounter: Int = 0;
		var argIndex: Int = -1;
	}

	protected def addField(
		productType: Type,
		productClassSymbol: ClassSymbol,
		fieldName: String,
		fieldType: Type,
		oFieldDefaultValue: Option[Tree],
		initialCoproductType: Type,
		initialHandler: Handler,
		metaConsideredFields: mutable.Map[String, ConsideredField],
		ics: InterAddFieldCallsState
	): Unit = {
		ics.argIndex += 1;
		val fieldSymbol = fieldType.typeSymbol;

		val (isRequired, defaultValue_expression) =
			oFieldDefaultValue match {
				case None =>
					if (fieldSymbol == definitions.OptionClass) {
						(false, q"Some(None)")

					} else {
						ics.requiredFieldsCounter += 1;
						(true, q"None")
					}
				case Some(defaultValue) =>
					(false, q"Some($defaultValue)")
			}

		val addProductField_snippet =
			metaConsideredFields.get(fieldName) match {

				case Some(ConsideredField(fieldType, firstOwnerName, consideredFieldIndex, consideredFieldBitSlot)) =>
					if (isRequired) {
						ics.requiredFieldsAccum = ics.requiredFieldsAccum.add(consideredFieldBitSlot);
					}

					if (fieldType =:= fieldType) {
						q"""state.addProductField(CpFieldInfo($fieldName, $consideredFieldIndex, ${ics.argIndex}, $defaultValue_expression));"""
					} else {
						val msg = s"""Unsupported situation while building a `Parser[$initialCoproductType]`: two implementations, `$productClassSymbol` and `$firstOwnerName`, have a field with the same name ("$fieldName") but different type."""
						initialHandler.setFailed(msg);
						ctx.abort(ctx.enclosingPosition, msg)
					}

				case None =>
					val consideredFieldBitSlot = ics.nextBitSlot;
					ics.nextBitSlot = ics.nextBitSlot.shifted;

					if (isRequired) {
						ics.requiredFieldsAccum = ics.requiredFieldsAccum.add(consideredFieldBitSlot);
					}

					val consideredFieldIndex = metaConsideredFields.size;
					metaConsideredFields.put(fieldName, ConsideredField(fieldType, productClassSymbol.fullName, consideredFieldIndex, consideredFieldBitSlot))

					val fieldParserExpression: ctx.Tree =
						if (fieldSymbol.isClass) {
							Handler.parserHandlersMap.get(new TypeKey(fieldType)) match {

								case Some(fieldHandler) =>
									initialHandler.addDependency(fieldHandler);
									q"""parsersBuffer(${fieldHandler.typeIndex}).get"""

								case None =>
									q"Parser[$fieldType]"
							}
						} else {
							q"Parser[$fieldType]"
						}



					q"""
state.addProductField(CpFieldInfo($fieldName, $consideredFieldIndex, ${ics.argIndex}, $defaultValue_expression));
state.addConsideredField(CpConsideredField($fieldName, $consideredFieldIndex, BitSet.BitSlot(${consideredFieldBitSlot.shardMask}, ${consideredFieldBitSlot.shardIndex}), $fieldParserExpression));"""
			}
		ics.addFieldTreeSeqBuilder.addOne(addProductField_snippet);

	}
}



