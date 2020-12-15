package jsfacile.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import jsfacile.read.Parser


/** @tparam P the type of the concrete data type for which this macro materializes a [[Parser]]*/
class ProductParserMacro[P, Ctx <: blackbox.Context](context: Ctx) extends ParserGenCommon(context) {
	import ctx.universe._

	/** Macro implicit materializer of [[ProductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl(productType: Type, productClassSymbol: ClassSymbol): ctx.Expr[Parser[P]] = {

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
												productHandler.setFailed(msg);
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

				ctx.info(ctx.enclosingPosition, s"product parser unchecked builder for ${show(productType)}: ${show(createParserCodeLines)}\n------${showParserDependencies(productHandler)}\n$showEnclosingMacros", force = false);
				// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[parserHandlersMap]], and this macro execution needs to know of them later.
				ctx.typecheck(createParserCodeLines/*.duplicate*/); // the duplicate is necessary because, according to Dymitro Mitin, the typeCheck method mutates its argument sometimes.
				productHandler.isCapturingDependencies = false; // this line must be immediately after the manual type-check
				ctx.info(ctx.enclosingPosition, s"product parser after builder check for ${show(productType)}", force = false);

				productHandler

			case Some(productHandler) =>
				registerParserDependency(productHandler);
				productHandler

		};

		this.buildBody[P](productType, productHandler);
	}
}
