package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.{ReflectTools, discriminatorField}
import jsfacile.macros.CoproductParserHelper.{CphProductInfo, FieldName}
import jsfacile.read.Parser

trait CoproductParserHelper[C <: CoproductUpperBound] {
	def fullName: String;
	def discriminator: FieldName;
	def productsInfo: ArraySeq[CphProductInfo[C]]
	def fieldsInfo: Map[FieldName, Parser[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	final case class CphFieldInfo[+V](name: FieldName, oDefaultValue: Option[V])
	final case class CphProductInfo[+P](name: ProductName, numberOfRequiredFields: Int, fields: Seq[CphFieldInfo[Any]], constructor: Seq[Any] => P);

	/** Traits for which the [[jsfacile.read]] package provides an implicit [[Parser]]. */
	val traitsForWhichTheReadPackageProvidesAnImplicitParser: Set[String] = Set(
		classOf[scala.Option[Any]].getName, // by jpOption
		classOf[scala.collection.Iterable[Any]].getName, // by jpIterable
		classOf[scala.collection.Map[Any, Any]].getName, // by jpUnsortedMap
		classOf[scala.collection.SortedMap[_, Any]].getName // by jpSortedMap
	);

	final class CpHelper[C <: CoproductUpperBound](val fullName: String, val discriminator: FieldName, val productsInfo: ArraySeq[CphProductInfo[_ <: C]], val fieldsInfo: Map[FieldName, Parser[_]]) extends CoproductParserHelper[C]

	final class CpHelperLazy extends CoproductParserHelper[CoproductUpperBound] with Lazy {
		private var instance: CoproductParserHelper[CoproductUpperBound] = _
		def set[C <: CoproductUpperBound](helper: CoproductParserHelper[C]): Unit = this.instance = helper.asInstanceOf[CoproductParserHelper[CoproductUpperBound]];
		def get[C <: CoproductUpperBound]: CoproductParserHelper[C] = this.asInstanceOf[CoproductParserHelper[C]];
		override def isEmpty: Boolean = instance == null;
		override def fullName: String = instance.fullName;
		override def discriminator: FieldName = instance.discriminator;
		override def productsInfo: ArraySeq[CphProductInfo[CoproductUpperBound]] = instance.productsInfo;
		override def fieldsInfo: Map[FieldName, Parser[CoproductUpperBound]] = instance.fieldsInfo.asInstanceOf[Map[FieldName, Parser[CoproductUpperBound]]];
	}

	val cpHelpersBuffer: mutable.ArrayBuffer[CpHelperLazy] = mutable.ArrayBuffer.empty;

	/** Macro implicit materializer of [[CoproductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materialize[C <: CoproductUpperBound]: CoproductParserHelper[C] = macro materializeImpl[C]

	def materializeImpl[C <: CoproductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._

		/** Tell for which types is a [[Parser]] already provided in the [[jsfacile.read]] package, in order to avid letting the [[jsfacile.read.jpCoproduct]] generate another and cause ambiguity error. */
		def doesTheReadPackageProvideAnImplicitParserFor(classSymbol: ClassSymbol): Boolean = {
			classSymbol.baseClasses.exists(bc => traitsForWhichTheReadPackageProvidesAnImplicitParser.contains(bc.fullName))
		}

		/** Adds to the `productsSnippetBuilder` a snippet for each product that extends the specified trait (or abstract class). */
		def addProductsBelongingTo(
			coproductClassSymbol: ClassSymbol,
			coproductType: Type,
			superHandler: Handler,
			productsSnippetsBuilder: mutable.Builder[Tree, Seq[Tree]]
		): Unit = {
			for {
				productSymbol <- coproductClassSymbol.knownDirectSubclasses
			} {
				val productClassSymbol = productSymbol.asClass;
				ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productClassSymbol.toTypeConstructor) match {
					case Right(productType) =>
						if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This is and edge case that occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and, therefore, not considered added to the products info set.

							if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields nor constructor.
								productsSnippetsBuilder.addOne(
									q"""
productsInfoBuilder.addOne(CphProductInfo(
	${productClassSymbol.name.toString},
	0,
	ArraySeq.empty[CphFieldInfo[Any]],
	(args: Seq[Any]) => ${productClassSymbol.module}
));"""
								)

							} else if (productClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
								if (productClassSymbol.isSealed) {
									addProductsBelongingTo(productClassSymbol, productType, superHandler, productsSnippetsBuilder)
								} else {
									ctx.abort(ctx.enclosingPosition, s"$productClassSymbol should be sealed")
								}

							} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose constructor is its primary one, and its fields are its primary constructor parameters.
								val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).paramLists

								val forEachFieldSnippet = Seq.newBuilder[ctx.Tree];
								var requiredFieldsCounter: Int = 0;
								var argIndex = -1;
								val ctorArgumentsTrees =
									for (params <- productCtorParamsLists) yield {
										for (param <- params) yield {
											val paramType = param.typeSignature.dealias
											val paramTypeSymbol = paramType.typeSymbol;
											argIndex += 1;

											val oDefaultValue =
												if (paramTypeSymbol.fullName == "scala.Option") {
													q"Some(None)"
												} else {
													requiredFieldsCounter += 1;
													q"None"
												}

											val paramParserExpression: ctx.Tree =
												if (paramTypeSymbol.isClass) {
													parserHandlersMap.get(new TypeKey(paramType)) match {
														case Some(paramHandler) =>
															superHandler.addDependency(paramHandler);

															if (!paramTypeSymbol.isAbstract) {
																q"""new _root_.jsfacile.read.ProductParser[$paramType](ppHelpersBuffer(${paramHandler.typeIndex}).get[$paramType])"""
															} else if (paramTypeSymbol.asClass.isSealed) {
																q"""new _root_.jsfacile.read.CoproductParser[$paramType](cpHelpersBuffer(${paramHandler.typeIndex}).get[$paramType])"""
															} else {
																ctx.abort(ctx.enclosingPosition, "Unreachable")
															}
														case None =>
															q"Parser[$paramType]"
													}
												} else {
													q"Parser[$paramType]"
												}

											forEachFieldSnippet.addOne(
												q"""
fieldsInfoBuilder.addOne((${param.name.toString}, $paramParserExpression));
productFieldsSeqBuilder.addOne(CphFieldInfo(${param.name.toString}, $oDefaultValue));""");
											q"args($argIndex).asInstanceOf[$paramType]";
										}
									}
								val ctorFunction = q"(args: Seq[Any]) => new $productClassSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

								productsSnippetsBuilder.addOne(
									q"""
..${forEachFieldSnippet.result()}
productsInfoBuilder.addOne(CphProductInfo(${productClassSymbol.name.toString}, $requiredFieldsCounter, productFieldsSeqBuilder.result(), $ctorFunction));
productFieldsSeqBuilder.clear();"""
								)
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
			ctx.abort(ctx.enclosingPosition, s"$coproductClassSymbol is not sealed")
		}
		// Avoid generating parsers that are already provided in the [[jsfacile.read]] package. They have the same precedence than [[jsfacile.read.jpCoproduct]] and would cause ambiguity error.
		if (doesTheReadPackageProvideAnImplicitParserFor(coproductClassSymbol)) {
			ctx.abort(ctx.enclosingPosition, s"""A parser for $coproductClassSymbol is already provided in the "jsfacile.read" package.""")
		}

		ctx.echo(ctx.enclosingPosition, s"Coproduct parser helper start for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}")

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductHandler = parserHandlersMap.get(coproductTypeKey) match {
			case None =>
				val cpHelperIndex = parserHandlersMap.size;
				val coproductHandler = new Handler(cpHelperIndex);
				registerParserDependency(coproductHandler);
				parserHandlersMap.put(coproductTypeKey, coproductHandler);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorFieldName: String = discriminatorField.parse(ctx.universe)(coproductClassSymbol)._1;

				val productsSnippetsBuilder = Seq.newBuilder[ctx.universe.Tree];
				addProductsBelongingTo(coproductClassSymbol, coproductType, coproductHandler, productsSnippetsBuilder);

				val cpHelperInitCodeLines =
					q"""
import _root_.scala.collection.immutable.{ArraySeq, Map};
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.CoproductParserHelper;
import CoproductParserHelper.{CphProductInfo, CphFieldInfo, CpHelper, cpHelpersBuffer};
import _root_.jsfacile.macros.ProductParserHelper.ppHelpersBuffer;

val proxy = cpHelpersBuffer($cpHelperIndex);
if (proxy.isEmpty) {
	val productsInfoBuilder = ArraySeq.newBuilder[CphProductInfo[_ <: $coproductType]];
	val fieldsInfoBuilder = Map.newBuilder[String, Parser[_]];
	val productFieldsSeqBuilder = ArraySeq.newBuilder[CphFieldInfo[Any]];

	..${productsSnippetsBuilder.result()}

	proxy.set(new CpHelper[$coproductType](${coproductType.toString}, $discriminatorFieldName, productsInfoBuilder.result(), fieldsInfoBuilder.result()));
}""";

				coproductHandler.oExpression = Some(cpHelperInitCodeLines);

				ctx.echo(ctx.enclosingPosition, s"coproduct parser helper unchecked init for ${show(coproductType)} : ${show(cpHelperInitCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				ctx.typecheck(cpHelperInitCodeLines);
				coproductHandler.isCapturingDependencies = false
				ctx.echo(ctx.enclosingPosition, s"coproduct appender helper after init check for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

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
					} yield handler.oExpression.get.asInstanceOf[ctx.Tree];

				q"""
import _root_.jsfacile.macros.CoproductParserHelper;
import CoproductParserHelper.{CpHelper, CpHelperLazy, cpHelpersBuffer};
import _root_.jsfacile.macros.ProductParserHelper.{PpHelperLazy, ppHelpersBuffer};

while(cpHelpersBuffer.size < ${parserHandlersMap.size}) {
	cpHelpersBuffer.addOne(new CpHelperLazy);
}
while(ppHelpersBuffer.size < ${parserHandlersMap.size}) {
	ppHelpersBuffer.addOne(new PpHelperLazy);
}
{..$inits}
cpHelpersBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""

			} else {
				q"""
import _root_.jsfacile.macros.CoproductParserHelper;

CoproductParserHelper.cpHelpersBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"coproduct parser helper unchecked body for ${show(coproductType)}: ${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
		val checkedBody = ctx.typecheck(body);
		ctx.echo(ctx.enclosingPosition, s"coproduct parser helper after body check for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		ctx.Expr[CoproductParserHelper[C]](checkedBody);
	}
}



