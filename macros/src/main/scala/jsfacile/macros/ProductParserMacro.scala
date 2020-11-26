package jsfacile.macros

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.read.ProductParser.PpFieldInfo
import jsfacile.read.{Cursor, Parser, ProductParser}


object ProductParserMacro {

	final class PpLazy extends Parser[ProductUpperBound] with Lazy {
		@volatile private var instance: ProductParser[ProductUpperBound] = _;
		def set[P](parser: ProductParser[P]): Unit = this.instance = parser.asInstanceOf[ProductParser[ProductUpperBound]];
		def get[P]: Parser[P] = this.asInstanceOf[Parser[P]];
		override def isEmpty: Boolean = instance == null;
		override def parse(cursor: Cursor): ProductUpperBound = this.instance.parse(cursor)
	}

	val fieldsOrdering: Ordering[PpFieldInfo] = Ordering.by(_.name);

	val ppBuffer: mutable.ArrayBuffer[PpLazy] = mutable.ArrayBuffer.empty;

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

											if (!paramTypeSymbol.isAbstract) {
												q"""ppBuffer(${paramHandler.typeIndex}).get"""
											} else if (paramTypeSymbol.asClass.isSealed) {
												q"""cpBuffer(${paramHandler.typeIndex}).get"""
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
				val ppHelperInitCodeLines =
					q"""
import _root_.scala.Array;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.scala.collection.immutable.Seq;

import _root_.jsfacile.read.{Parser, ProductParser};
import ProductParser.{PpFieldInfo, PpHelper};
import _root_.jsfacile.macros.CoproductParserMacro.cpBuffer;
import _root_.jsfacile.macros.ProductParserMacro.{ppBuffer, fieldsOrdering};

val proxy = ppBuffer($ppHelperIndex);
if (proxy.isEmpty) {
	val builder = ArrayBuffer[PpFieldInfo]();
	..${addFieldInfoSnippetsBuilder.result()}

	val helper = new PpHelper[$productType] {
		override val fullName: String = ${productType.toString};

		override val fieldsInfo: Array[PpFieldInfo] = builder.sortInPlace()(fieldsOrdering).toArray;

		override def createProduct(args: Seq[Any]):$productType = new $productSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
	};
 	proxy.set(new ProductParser[$productType](helper));
}""";
				productHandler.oExpression = Some(ppHelperInitCodeLines);

				ctx.echo(ctx.enclosingPosition, s"product parser unchecked init for ${show(productType)}: ${show(ppHelperInitCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				ctx.typecheck(ppHelperInitCodeLines.duplicate); // the duplicate is necessary because, according to Dymitro Mitin, the typeCheck method mutates its argument sometimes.
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
					} yield handler.oExpression.get.asInstanceOf[ctx.Tree];

				q"""
import _root_.jsfacile.macros.CoproductParserMacro.{CpLazy, cpBuffer};
import _root_.jsfacile.macros.ProductParserMacro.{PpLazy, ppBuffer}
import _root_.jsfacile.macros.parsersBufferSemaphore;

if (ppBuffer.size < ${parserHandlersMap.size}) {
	parsersBufferSemaphore.synchronized {
		while(cpBuffer.size < ${parserHandlersMap.size}) {
			cpBuffer.addOne(new CpLazy);
		}
		while(ppBuffer.size < ${parserHandlersMap.size}) {
			ppBuffer.addOne(new PpLazy);
		}
	}
}
{..$inits}
ppBuffer(${productHandler.typeIndex}).get[$productType]"""

			} else {
				q"""_root_.jsfacile.macros.ProductParserMacro.ppBuffer(${productHandler.typeIndex}).get[$productType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"product parser unchecked body for ${show(productType)}:\n${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
		ctx.Expr[Parser[P]](body);
	}
}
