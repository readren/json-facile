package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.api.builder.ProductParsingInfo
import jsfacile.macros.GenCommon.{FieldParsingInfo, ProductCustomization, ProductParsingInfoBuilderState}
import jsfacile.read.Parser
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot


class ParserBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductParserMacro(context) {
	import ctx.universe._

	def addFieldToParsingInfo[P, F](coproductType: Type, productType: Type, fieldType: Type, nameExpr: ctx.Expr[String], defaultValueExpr: ctx.Expr[Option[F]]): ctx.Expr[Unit] = {
		getClassSymbol(coproductType);
		getClassSymbol(productType);

		nameExpr.tree match {
			case Literal(Constant(fieldName: String)) =>


				// convert `Expr[Option[F]]` to `Option[Expr[F]]`
				val oDefaultValue = defaultValueExpr.tree match {
					case q"scala.Some.apply[$_]($dv)" => Some(dv)
					case q"scala.None" => None
					case _ => ctx.abort(ctx.enclosingPosition, s"""The optional aspect of the `defaultValue` argument of the `ProductParsingInfoBuilder.add` method must be expressed literally. In other words, said argument expression form must match either of these templates: `Some(<expression>)` or `None`.""")
				}

				val coproductTypeKey = new TypeKey(coproductType);
				val coproductBuilderState = getCoproductBuilderStateOf(coproductTypeKey)

				val infoBuilderState = coproductBuilderState.parsingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductParsingInfoBuilderState[P])
				infoBuilderState.fields.addOne(new FieldParsingInfo(fieldName, fieldType, oDefaultValue))

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"The `name` argument of the `ProductParsingInfoBuilder.add` method must be a literal string. A expression is not supported.")
		}
		ctx.Expr(q"()")
	}

	def completeProductParsingInfo[P](coproductType: Type, productType: Type, nameExpr: ctx.Expr[String], ctorExpr: ctx.Expr[Seq[Any] => P]): ctx.Expr[ProductParsingInfo[P]] = {

		nameExpr.tree match {
			case Literal(Constant(productName: String)) =>

				val coproductTypeKey = new TypeKey(coproductType);
				val coproductBuilderState = getCoproductBuilderStateOf(coproductTypeKey)

				val infoBuilderState = coproductBuilderState.parsingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductParsingInfoBuilderState[P]).asInstanceOf[ProductParsingInfoBuilderState[P]]
				infoBuilderState.name = Some(productName);
				infoBuilderState.ctorExpr = Some(ctorExpr)

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"The `name` argument of the `ProductParsingInfoBuilder.complete` method must be a literal string. A expression is not supported.")

		}
		ctx.Expr(q"new _root_.jsfacile.api.builder.ProductParsingInfo[$productType]()")
	}



	def sealParser(coproductType: Type): ctx.Expr[Parser[C]] = {
		val coproductSymbol = getClassSymbol(coproductType);

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductBuilderState = getCoproductBuilderStateOf(coproductTypeKey);
		val coproductHandler = getCleanHandlerFor(coproductTypeKey, parserHandlersMap);

		val productAdditionTrees: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty;
		var nextBitSlot: BitSlot = BitSet.FIRST_BIT_SLOT;
		val metaConsideredFields = mutable.Map.empty[String, ConsideredField];
		for ((productType, productConfig) <- coproductBuilderState.productsCollector.asInstanceOf[mutable.Map[Type, ProductCustomization]]) {

			val productSymbol = productType.typeSymbol.asClass;
			productConfig.oParsingInfo match {

				case None =>
					nextBitSlot = this.addProduct(productSymbol.asClass, coproductSymbol, coproductType, coproductType, coproductHandler, nextBitSlot, metaConsideredFields, productAdditionTrees);

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
				this.buildBody[C](coproductType, coproductHandler);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")
		}
	}
}
