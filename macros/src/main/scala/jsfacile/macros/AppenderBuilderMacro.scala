package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.api.builder.ProductAppendingInfo
import jsfacile.macros.GenCommon.{FieldAppendingInfo, ProductAppendingInfoBuilderState, ProductAppendingInfoDigested_fromBuilder, ProductAppendingInfoDigested_handmade, ProductCustomization, TypeKey}
import jsfacile.macros.Handler.appenderHandlersMap
import jsfacile.write.Appender

class AppenderBuilderMacro[C, Ctx <: blackbox.Context](context: Ctx) extends CoproductAppenderMacro(context) {

	import ctx.universe._


	def addFieldToAppendingInfo[P, F](coproductType: Type, productType: Type, fieldType: Type, nameExpr: ctx.Expr[String], accessorExpr: ctx.Expr[P => F]): ctx.Expr[Unit] = {
		getClassSymbol(coproductType);
		getClassSymbol(productType);

		nameExpr.tree match {
			case Literal(Constant(fieldName: String)) =>

				val coproductTypeKey = new TypeKey(coproductType);
				val translatorsBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey)

				val infoBuilderState = translatorsBuilderState.appendingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductAppendingInfoBuilderState)
				infoBuilderState.fields.addOne(new FieldAppendingInfo(fieldName, fieldType, accessorExpr.tree))

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"The `name` argument of the `ProductAppendingInfoBuilder.add` method must be a literal string. An expression is not supported.")
		}
		ctx.Expr(q"()")
	}

	def completeProductAppendingInfo[P](coproductType: Type, productType: Type, oDiscriminatorValueExpr: Option[ctx.Expr[String]]): ctx.Expr[ProductAppendingInfo[P]] = {
		val discriminatorValue = oDiscriminatorValueExpr match {
			case None =>
				productType.typeSymbol.name.toString

			case Some(discriminatorValueExpr) =>
				discriminatorValueExpr.tree match {

					case Literal(Constant(discriminatorValue: String)) =>
						discriminatorValue

					case _ =>
						ctx.abort(ctx.enclosingPosition, s"The `discriminatorValue` argument of the `ProductAppendingInfoBuilder.complete` method must be a literal string. A expression is not supported.")
				}
		}

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey)

		val infoBuilderState = coproductBuilderState.appendingInfoBuilderStatePerProduct.getOrElseUpdate(productType, new ProductAppendingInfoBuilderState)
		infoBuilderState.discriminatorValue = Some(discriminatorValue);

		ctx.Expr(q"new _root_.jsfacile.macros.ProductAppendingInfoImpl[$productType]()")
	}

	def sealAppender(coproductType: Type): ctx.Expr[Appender[C]] = {
		val initialCoproductSymbol = getClassSymbol(coproductType);

		/** Discard the appenders generated in other code contexts. This is necessary because: (1) Since the existence of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] the derived [[Appender]]s depends on the context; and (2) the existence of an [[Appender]] in the implicit scope depends on the context. */
		Handler.appenderHandlersMap.clear()

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductTranslatorsBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey);
		val coproductHandler = Handler.getHandlerFor(coproductTypeKey, appenderHandlersMap)

		val productsInfoCollector: mutable.ArrayBuffer[ProductInfo] = mutable.ArrayBuffer.empty;
		for {(productType, productCustomization) <- coproductTranslatorsBuilderState.productsCollector.asInstanceOf[mutable.Map[Type, ProductCustomization]]} {
			productCustomization.oAppendingInfo match {

				case None => // if no appender customization is provided for this product, derive the appender from the set of fields in its primary constructor
					val productSymbol = productType.typeSymbol;
					this.addSubtype(productSymbol.asClass, initialCoproductSymbol, coproductType, coproductHandler, productsInfoCollector);

				case Some(productInfo: ProductAppendingInfoDigested_handmade) => // if a handmade appender customization was provided, use the specified appender
					productsInfoCollector.addOne(CustomProductInfo(productType, productInfo.requiredFieldNames, productInfo.appenderTree.asInstanceOf[Tree]))

				case Some(productInfo: ProductAppendingInfoDigested_fromBuilder) => // if the appender customization info was built with a ProductAppendingInfoBuilder, derive the appender from the specified set of fields.

					val requiredFieldsSetBuilder = Set.newBuilder[String]
					var isFirstField = true;
					val appendField_codeLines: Iterable[Tree] =
						for (field <- productInfo.fields) yield {
							if(field.tpe.typeSymbol != definitions.OptionClass) {
								requiredFieldsSetBuilder.addOne(field.name)
							}
							val sb = new StringBuilder(field.name.length + 4)
							if (isFirstField) {
								isFirstField = false
							} else {
								sb.append(',');
							}

							sb.append('"').append(field.name).append('"').append(':');
							q"""r.append(${sb.toString}).appendSummoned[${field.tpe.asInstanceOf[Type]}](${field.accessor.asInstanceOf[Tree]}(p))"""
						}
					productsInfoCollector.addOne(DerivedProductInfo(productInfo.discriminatorValue, productType, requiredFieldsSetBuilder.result(), appendField_codeLines))
			}
		}

		coproductHandler.creationTreeOrErrorMsg match {
			case None =>
				buildAppenderCreationTreeOn(coproductType, coproductHandler, None, productsInfoCollector);
				this.buildBody[C](coproductType, coproductHandler, isOuterMacroInvocation = true);

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				throw new AssertionError(s"Unreachable reached: coproductType=$coproductType.")

		}
	}
}
