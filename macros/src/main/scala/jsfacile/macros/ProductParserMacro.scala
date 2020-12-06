package jsfacile.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import jsfacile.read.Parser


class ProductParserMacro[Ctx <: blackbox.Context](val ctx: Ctx) {
	import ctx.universe._

	/** Macro implicit materializer of [[ProductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl[P](productType: Type, productClassSymbol: ClassSymbol): ctx.Expr[Parser[P]] = {

//		ctx.info(ctx.enclosingPosition, s"product parser helper start for ${show(productType)}", force = false);

		val productTypeKey = new TypeKey(productType);
		val productHandler = parserHandlersMap.get(productTypeKey) match {

			case None =>
				val ppHelperIndex = parserHandlersMap.size;
				val productHandler = new Handler(ppHelperIndex);
				registerParserDependency(productHandler);
				parserHandlersMap.put(productTypeKey, productHandler);

				val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
				val paramsList = productClassSymbol.asClass.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;
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
												val msg = s"Unreachable reached: productType=$productType, paramType=$paramType";
												productHandler.creationTreeOrErrorMsg = Some(Left(msg));
												ctx.abort(ctx.enclosingPosition, msg)
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

val createParser: Array[LazyParser] => ProductParser[$productType] = parsersBuffer => {
	val builder = ArrayBuffer[PpFieldInfo]();
	..${addFieldInfoSnippetsBuilder.result()}

	val helper = new PpHelper[$productType] {
		override val fullName: String = ${productType.toString};

		override val fieldsInfo: Array[PpFieldInfo] = builder.sortInPlace()(ProductParser.fieldsOrdering).toArray;

		override def createProduct(args: Seq[Any]):$productType = new $productClassSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
	}
	new ProductParser[$productType](helper)
};
createParser""";

				productHandler.creationTreeOrErrorMsg = Some(Right(createParserCodeLines));

				ctx.info(ctx.enclosingPosition, s"product parser unchecked builder for ${show(productType)}: ${show(createParserCodeLines)}\n------${showParserDependencies(productHandler)}\n${showEnclosingMacros(ctx)}", force = false);
				ctx.typecheck(createParserCodeLines); // the duplicate is necessary because, according to Dymitro Mitin, the typeCheck method mutates its argument sometimes.
				productHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				ctx.info(ctx.enclosingPosition, s"product parser after builder check for ${show(productType)}", force = false);

				productHandler

			case Some(productHandler) =>
				registerParserDependency(productHandler);
				productHandler

		};

		val body =
			if (productHandler.creationTreeOrErrorMsg.isDefined && isOuterParserMacroInvocation(ctx)) {
				val inits =
					for {
						(innerTypeKey, innerHandler) <- parserHandlersMap
						if productHandler.doesDependOn(innerHandler.typeIndex)
					} yield {
						innerHandler.creationTreeOrErrorMsg.get match {
							case Right(creationTree) =>
								val createParserCodeLines = creationTree.asInstanceOf[ctx.Tree];
								q"""parsersBuffer(${innerHandler.typeIndex}).set($createParserCodeLines(parsersBuffer));"""

							case Left(innerErrorMsg) =>
								ctx.abort(ctx.enclosingPosition, s"Unable to derive a parser for $productType because it depends on the parser for ${innerTypeKey.toString} whose derivation has failed: $innerErrorMsg.")

						}

					}
				q"""
import _root_.jsfacile.macros.LazyParser;

val parsersBuffer = _root_.scala.Array.fill(${parserHandlersMap.size})(new LazyParser);
{..$inits}
parsersBuffer(${productHandler.typeIndex}).get[$productType]"""

			} else {
				q"""parsersBuffer(${productHandler.typeIndex}).get[$productType]"""
			}

		ctx.info(ctx.enclosingPosition, s"product parser body for ${show(productType)}:\n${show(body)}\n------${showParserDependencies(productHandler)}\n${showEnclosingMacros(ctx)}", force = false);
		ctx.Expr[Parser[P]](body);
	}
}
