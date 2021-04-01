package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.api.builder.ProductParsingInfo
import jsfacile.joint.{CoproductsOnly, DiscriminatorValueMapper}
import jsfacile.macros.GenCommon.{FieldParsingInfo, ProductCustomization, ProductParsingInfoBuilderState, TypeKey}
import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot


class ParserBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductParserMacro(context) {
	import ctx.universe._

	def addFieldToParsingInfo[P, F](coproductType: Type, productType: Type, fieldType: Type, nameExpr: ctx.Expr[String], oDefaultValueExpr: Option[ctx.Expr[F]]): ctx.Expr[Unit] = {
		getClassSymbol(coproductType);
		getClassSymbol(productType);

		nameExpr.tree match {
			case Literal(Constant(fieldName: String)) =>

				val oDefaultValue = oDefaultValueExpr.map(_.tree)
				val coproductTypeKey = new TypeKey(coproductType);
				val translatorsBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey)

				val infoBuilderState = translatorsBuilderState.parsingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductParsingInfoBuilderState[P])
				infoBuilderState.fields.addOne(new FieldParsingInfo(fieldName, fieldType, oDefaultValue))

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"The `name` argument of the `ProductParsingInfoBuilder.add` method must be a literal string. A expression is not supported.")
		}
		ctx.Expr(q"()")
	}

	def completeProductParsingInfo[P](coproductType: Type, productType: Type, ctorExpr: ctx.Expr[Seq[Any] => P], oDiscriminatorValueExpr: Option[ctx.Expr[String]]): ctx.Expr[ProductParsingInfo[P]] = {

		val discriminatorValue = oDiscriminatorValueExpr match {
			case None =>
				productType.typeSymbol.name.toString

			case Some(discriminatorValueExpr) =>
				discriminatorValueExpr.tree match {
					case Literal(Constant(discriminatorValue: String)) =>
						discriminatorValue

					case _ =>
						ctx.abort(ctx.enclosingPosition, s"The `discriminatorValue` argument of the `ProductParsingInfoBuilder.complete` method must be a literal string. A expression is not supported.")
				}
		}

		val coproductTypeKey = new TypeKey(coproductType);
		val translatorsBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey)

		val infoBuilderState = translatorsBuilderState.parsingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductParsingInfoBuilderState[P]).asInstanceOf[ProductParsingInfoBuilderState[P]]
		infoBuilderState.discriminatorValue = Some(discriminatorValue);
		infoBuilderState.ctorExpr = Some(ctorExpr)

		ctx.Expr(q"new _root_.jsfacile.macros.ProductParsingInfoImpl[$productType]")
	}



	def sealParser(coproductType: Type): ctx.Expr[Parser[C]] = {
		val coproductSymbol = getClassSymbol(coproductType);

		/** Discard the [[Parser]]s generated in other code contexts. This is necessary because: (1) since the existence of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] the derived [[Parser]]s depends on the context; and (2) the existence of an [[Parser]] in the implicit scope depends on the context. */
		Handler.parserHandlersMap.clear();

		val coproductTypeKey = new TypeKey(coproductType);
		val translatorsBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey);

		ctx.info(ctx.enclosingPosition, s"Creating a Parser[$coproductType] considering the following subtypes: ${translatorsBuilderState.productsCollector.keys.map{x => x.toString}.toSeq.sorted.mkString("{\n", ",\n","\n}\n----")}", true)

		val coproductHandler = Handler.getHandlerFor(coproductTypeKey, Handler.parserHandlersMap);

		val discriminatorValueMapperType = appliedType(typeOf[DiscriminatorValueMapper[_, _]].typeConstructor, List(coproductType, typeOf[CoproductsOnly]));
		val discriminatorValueMapperInstance = ctx.inferImplicitValue(discriminatorValueMapperType, silent = true, withMacrosDisabled = true)

		val productAdditionTrees: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty;
		var nextBitSlot: BitSlot = BitSet.FIRST_BIT_SLOT;
		val metaConsideredFields = mutable.Map.empty[String, ConsideredField];
		for ((productType, productConfig) <- translatorsBuilderState.productsCollector.asInstanceOf[mutable.Map[Type, ProductCustomization]]) {

			val productSymbol = productType.typeSymbol.asClass;
			productConfig.oParsingInfo match {

				case None =>
					nextBitSlot = this.addSubtype(productSymbol, coproductSymbol, coproductType, coproductType, discriminatorValueMapperInstance, coproductHandler, nextBitSlot, metaConsideredFields, productAdditionTrees);

				case Some(parsingInfo) =>
					val ics = new InterAddFieldCallsState(nextBitSlot);
					for {fieldInfo <- parsingInfo.fieldsInfo} {
						this.addField(productType, productSymbol, fieldInfo.name, fieldInfo.tpe.asInstanceOf[Type], fieldInfo.oDefaultValue.asInstanceOf[Option[Tree]], coproductType, coproductHandler, metaConsideredFields, ics);
					}
					nextBitSlot = ics.nextBitSlot;

					productAdditionTrees.addOne(
						q"""
..${ics.addFieldTreeSeqBuilder.result()}
state.addProduct(CpProductInfo[$coproductType](
	${productSymbol.name.toString},
	new BitSet(Array(..${new ArraySeq.ofLong(ics.requiredFieldsAccum.shards)})),
	${ics.requiredFieldsCounter},
	state.productFields,
	${parsingInfo.ctor.asInstanceOf[Tree]}
));
state.productFieldsBuilder.clear();"""
					)
			}
		}

		coproductHandler.creationTreeOrErrorMsg match {
			case None =>
				this.buildParserCreationTreeOn(coproductHandler, coproductType, coproductSymbol, productAdditionTrees, nextBitSlot.shardIndex + 1)
				this.buildBody[C](coproductType, coproductHandler, isOuterMacroInvocation = true);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")
		}
	}
}
