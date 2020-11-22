package jsfacile.macros

import java.util.Comparator

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.read.Parser


trait ProductParserHelper[P <: ProductUpperBound] {
	def fullName: String;
	def fieldsInfo: Array[ProductParserHelper.PphFieldInfo[_]];
	def createProduct(args: Seq[Any]): P
}

object ProductParserHelper {

	/** TODO: make it covariant on F if [[Parser]] is made covariant. */
	final case class PphFieldInfo[F](name: String, valueParser: Parser[F], oDefaultValue: Option[F], ctorArgIndex: Int)

	/** Compares two [[PphFieldInfo]] based solely on their names. */
	val fieldInfoComparator: Comparator[PphFieldInfo[_]] = { (a, b) => a.name.compare(b.name) }

	/** Concrete classes for which the [[jsfacile.read]] package provides an implicit [[Parser]]. */
	val concreteClassesForWhichTheReadPackageProvidesAnImplicitParser: Set[String] = Set(
		classOf[java.lang.String].getName, // by jpString
		classOf[scala.math.BigDecimal].getName, // by jpBigDecimal
		classOf[scala.math.BigInt].getName, // by jpBigInt
		classOf[scala.Some[Any]].getName // by jpSome
	);

	final class PpHelperLazy extends ProductParserHelper[ProductUpperBound] with Lazy {
		@volatile private var instance: ProductParserHelper[ProductUpperBound] = _;
		def set[P](helper: ProductParserHelper[P]): Unit = this.instance = helper.asInstanceOf[ProductParserHelper[ProductUpperBound]];
		def get[P]: ProductParserHelper[P] = this.asInstanceOf[ProductParserHelper[P]];
		override def isEmpty: Boolean = instance == null;
		override def fullName: String = instance.fullName;
		override def fieldsInfo: Array[PphFieldInfo[_]] = instance.fieldsInfo;
		override def createProduct(args: Seq[Any]): ProductUpperBound = instance.createProduct(args);
	}

	val ppHelpersBuffer: mutable.ArrayBuffer[PpHelperLazy] = mutable.ArrayBuffer.empty;

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materialize[P <: ProductUpperBound]: ProductParserHelper[P] = macro materializeImpl[P]

	def materializeImpl[P <: ProductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[ProductParserHelper[P]] = {
		import ctx.universe._

		/** Used by the [[ProductParserHelper.materialize]] macro to avoid generating parsers for types that are already provided by this package. */
		def doesTheReadPackageProvideAnImplicitParserFor(classSymbol: ClassSymbol): Boolean = {
			classSymbol.baseClasses.exists(bc => concreteClassesForWhichTheReadPackageProvidesAnImplicitParser.contains(bc.fullName))
		}

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (!productSymbol.isClass || productSymbol.isAbstract || productSymbol.isModuleClass) {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol is not a concrete non module class")
		}
		val classSymbol = productSymbol.asClass;
		if (doesTheReadPackageProvideAnImplicitParserFor(classSymbol)) {
			ctx.abort(ctx.enclosingPosition, s"""A parser for "$productSymbol" is already provided in the "jsfacile.read" package.""")
		}

		ctx.echo(ctx.enclosingPosition, s"product parser helper start for ${show(productType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		val productTypeKey = new TypeKey(productType);
		val productHandler = parserHandlersMap.get(productTypeKey) match {

			case None =>
				val ppHelperIndex = parserHandlersMap.size;
				val productHandler = new Handler(ppHelperIndex);
				registerParserDependency(productHandler);
				parserHandlersMap.put(productTypeKey, productHandler);

				val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

				val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
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


							addFieldInfoSnippetsBuilder.addOne(
								q"""builder.addOne(PphFieldInfo(${param.name.toString}, $paramParserExpression, $oDefaultValue, $argIndex));"""
							);
							q"args($argIndex).asInstanceOf[$paramType]";
						}
					}
				val ppHelperInitCodeLines =
					q"""
import _root_.scala.Array;
import _root_.scala.collection.immutable.Seq;
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.CoproductParserHelper.cpHelpersBuffer;
import _root_.jsfacile.macros.ProductParserHelper;
import ProductParserHelper.{PphFieldInfo, fieldInfoComparator, ppHelpersBuffer};

val proxy = ppHelpersBuffer($ppHelperIndex);
if (proxy.isEmpty) {
	val builder = Array.newBuilder[PphFieldInfo[_]];
	..${addFieldInfoSnippetsBuilder.result()}

	val fieldsArray = builder.result();
	_root_.java.util.Arrays.sort(fieldsArray, fieldInfoComparator);

	proxy.set(new ProductParserHelper[$productType] {
		override val fullName: String = ${productType.toString};

		override val fieldsInfo: Array[PphFieldInfo[_]] = fieldsArray;

		override def createProduct(args: Seq[Any]):$productType = new $productSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
	})
}""";
				ctx.echo(ctx.enclosingPosition, s"product parser helper unchecked init for ${show(productType)}: ${show(ppHelperInitCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				// the recursion is triggered by the type-check
				productHandler.oExpression = Some(ctx.Expr[Unit](ctx.typecheck(ppHelperInitCodeLines)));
				productHandler.isCapturingDependencies = false
				ctx.echo(ctx.enclosingPosition, s"product parser helper after init check for ${show(productType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

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
					} yield handler.oExpression.get.in(ctx.mirror);

				q"""
import _root_.jsfacile.macros.CoproductParserHelper.{CpHelperLazy, cpHelpersBuffer};
import _root_.jsfacile.macros.ProductParserHelper
import ProductParserHelper.{PpHelperLazy, ppHelpersBuffer}
import _root_.jsfacile.macros.parsersBufferSemaphore;

if (ppHelpersBuffer.size < ${parserHandlersMap.size}) {
	parsersBufferSemaphore.synchronized {
		while(cpHelpersBuffer.size < ${parserHandlersMap.size}) {
			cpHelpersBuffer.addOne(new CpHelperLazy);
		}
		while(ppHelpersBuffer.size < ${parserHandlersMap.size}) {
			ppHelpersBuffer.addOne(new PpHelperLazy);
		}
	}
}
{..$inits}
ppHelpersBuffer(${productHandler.typeIndex}).get[$productType]"""

			} else {
				q"""
import _root_.jsfacile.macros.ProductParserHelper
import ProductParserHelper.ppHelpersBuffer;

ppHelpersBuffer(${productHandler.typeIndex}).get[$productType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"product parser helper unchecked body for ${show(productType)}: ${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
		val checkedBody = ctx.typecheck(body);
		ctx.echo(ctx.enclosingPosition, s"product parser helper after body check for ${show(productType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		ctx.Expr[ProductParserHelper[P]](checkedBody);

	}
}
