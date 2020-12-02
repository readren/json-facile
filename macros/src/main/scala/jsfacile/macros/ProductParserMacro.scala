package jsfacile.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.ProductUpperBound
import jsfacile.read.ProductParser.PpFieldInfo
import jsfacile.read.Parser


object ProductParserMacro {

	val fieldsOrdering: Ordering[PpFieldInfo] = Ordering.by(_.name);

	/** Macro implicit materializer of [[ProductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl[P <: ProductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Parser[P]] = {
		import ctx.universe._

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (!productSymbol.isClass || productSymbol.isAbstract || productSymbol.isModuleClass) {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol is not a concrete non module class")
		}
		ctx.echo(ctx.enclosingPosition, s"product parser helper start for ${show(productType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		val productTypeKey = new TypeKey(productType);
		val productHandler = parserHandlersMap.get(productTypeKey) match {

			case None =>
				val ppHelperIndex = parserHandlersMap.size;
				val productHandler = new Handler(ppHelperIndex);
				registerParserDependency(productHandler);
				parserHandlersMap.put(productTypeKey, productHandler);

				val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
				val paramsList = productSymbol.asClass.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;
				var argIndex = -1;
				val ctorArgumentSnippets =
					for (params <- paramsList) yield {
						for (param <- params) yield {
							argIndex += 1;
							val paramType = param.typeSignature.dealias
							val paramTypeSymbol = paramType.typeSymbol;
							val oDefaultValue =
								if (paramTypeSymbol.fullName == "scala.Option") {
									q"Some(None)"
								} else {
									q"None"
								}

							val paramParserExpression =
								if (paramTypeSymbol.isClass) {
									parserHandlersMap.get(new TypeKey(paramType)) match {
										case Some(paramHandler) =>
											productHandler.addDependency(paramHandler);

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


							addFieldInfoSnippetsBuilder.addOne(
								q"""builder.addOne(PpFieldInfo(${param.name.toString}, $paramParserExpression, $oDefaultValue, $argIndex));"""
							);
							q"args($argIndex).asInstanceOf[$paramType]";
						}
					}
				val createParserCodeLines =
					q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.scala.collection.immutable.Seq;

import _root_.jsfacile.read.{Parser, ProductParser};
import ProductParser.{PpFieldInfo, PpHelper};
import _root_.jsfacile.macros.LazyParser;
import _root_.jsfacile.macros.ProductParserMacro.fieldsOrdering;

val createParser: Array[LazyParser] => ProductParser[$productType] = parsersBuffer => {
	val builder = ArrayBuffer[PpFieldInfo]();
	..${addFieldInfoSnippetsBuilder.result()}

	val helper = new PpHelper[$productType] {
		override val fullName: String = ${productType.toString};

		override val fieldsInfo: Array[PpFieldInfo] = builder.sortInPlace()(fieldsOrdering).toArray;

		override def createProduct(args: Seq[Any]):$productType = new $productSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
	}
	new ProductParser[$productType](helper)
};
createParser""";

				productHandler.oExpression = Some(createParserCodeLines);

				ctx.echo(ctx.enclosingPosition, s"product parser unchecked init for ${show(productType)}: ${show(createParserCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				ctx.typecheck(createParserCodeLines); // the duplicate is necessary because, according to Dymitro Mitin, the typeCheck method mutates its argument sometimes.
				productHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				ctx.echo(ctx.enclosingPosition, s"product parser after init check for ${show(productType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

				productHandler

			case Some(productHandler) =>
				registerParserDependency(productHandler);
				productHandler

		};

		val body =
			if (productHandler.oExpression.isDefined && isOuterParserMacroInvocation(ctx)) {
				val inits =
					for {
						(_, handler) <- parserHandlersMap
						if productHandler.doesDependOn(handler.typeIndex)
					} yield {
						val createParserCodeLines = handler.oExpression.get.asInstanceOf[ctx.Tree];
						q"""parsersBuffer(${handler.typeIndex}).set($createParserCodeLines(parsersBuffer));"""

					}
				q"""
import _root_.jsfacile.macros.LazyParser;

val parsersBuffer = _root_.scala.Array.fill(${parserHandlersMap.size})(new LazyParser);
{..$inits}
parsersBuffer(${productHandler.typeIndex}).get[$productType]"""

			} else {
				q"""parsersBuffer(${productHandler.typeIndex}).get[$productType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"product parser unchecked body for ${show(productType)}:\n${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
		ctx.Expr[Parser[P]](body);
	}
}
