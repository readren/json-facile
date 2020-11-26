package jsfacile.macros

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.{ReflectTools, discriminatorField}
import jsfacile.read.{CoproductParser, Cursor, Parser}

object CoproductParserMacro {

	final class CpLazy extends Parser[CoproductUpperBound] with Lazy {
		@volatile private var instance: CoproductParser[CoproductUpperBound] = _
		def set[C <: CoproductUpperBound](parser: CoproductParser[C]): Unit = this.instance = parser.asInstanceOf[CoproductParser[CoproductUpperBound]];
		def get[C <: CoproductUpperBound]: Parser[C] = this.asInstanceOf[Parser[C]];
		override def isEmpty: Boolean = this.instance == null;
		/** The implementation should never call another [[Parser]] instance passing the cursor in failed or missed state. And therefore, can asume that the received cursor is {{{cursor.ok == true}}}. */
		override def parse(cursor: Cursor): CoproductUpperBound = this.instance.parse(cursor);
	}

	val cpBuffer: mutable.ArrayBuffer[CpLazy] = mutable.ArrayBuffer.empty;

	/** Macro implicit materializer of [[CoproductParserMacro]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	def materializeImpl[C <: CoproductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[Parser[C]] = {
		import ctx.universe._

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
	Array.empty[CphFieldInfo],
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

											forEachFieldSnippet.addOne(
												q"""
fieldsParsersBuilder.addOne(CphFieldParser(${param.name.toString}, $paramParserExpression));
productFieldsBuilder.addOne(CphFieldInfo(${param.name.toString}, $argIndex, $oDefaultValue));""");
											q"args($argIndex).asInstanceOf[$paramType]";
										}
									}
								val ctorFunction = q"(args: Seq[Any]) => new $productClassSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

								productsSnippetsBuilder.addOne(
									q"""
..${forEachFieldSnippet.result()}
val productFieldsArray = productFieldsBuilder.sortInPlace()(namedOrdering.asInstanceOf[Ordering[CphFieldInfo]]).toArray;
productsInfoBuilder.addOne(CphProductInfo(${productClassSymbol.name.toString}, $requiredFieldsCounter, productFieldsArray, $ctorFunction));
productFieldsBuilder.clear();"""
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

		ctx.echo(ctx.enclosingPosition, s"Coproduct parser helper start for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}")

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductHandler = parserHandlersMap.get(coproductTypeKey) match {
			case None =>
				val cpTypeIndex = parserHandlersMap.size;
				val coproductHandler = new Handler(cpTypeIndex);
				registerParserDependency(coproductHandler);
				parserHandlersMap.put(coproductTypeKey, coproductHandler);

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorFieldName: String = discriminatorField.parse(ctx.universe)(coproductClassSymbol)._1;

				val productsSnippetsBuilder = Seq.newBuilder[ctx.universe.Tree];
				addProductsBelongingTo(coproductClassSymbol, coproductType, coproductHandler, productsSnippetsBuilder);

				val cpHelperInitCodeLines =
					q"""
import _root_.scala.collection.immutable.ArraySeq;
import _root_.scala.collection.mutable.ArrayBuffer;
import _root_.scala.math.Ordering;
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.CoproductParserMacro.cpBuffer;
import _root_.jsfacile.macros.ProductParserMacro.ppBuffer;
import _root_.jsfacile.macros.namedOrdering;
import _root_.jsfacile.read.CoproductParser;
import CoproductParser.{CphProductInfo, CphFieldInfo, CphFieldParser};

val proxy = cpBuffer($cpTypeIndex);
if (proxy.isEmpty) {
	val productsInfoBuilder = ArraySeq.newBuilder[CphProductInfo[_ <: $coproductType]];
	val fieldsParsersBuilder = ArrayBuffer[CphFieldParser]();
	val productFieldsBuilder = ArrayBuffer[CphFieldInfo]();

	..${productsSnippetsBuilder.result()}

	proxy.set(new CoproductParser[$coproductType](
		${coproductType.toString},
		$discriminatorFieldName,
		productsInfoBuilder.result(),
		fieldsParsersBuilder.sortInPlace()(namedOrdering.asInstanceOf[Ordering[CphFieldParser]]).toArray
	));
}""";

				coproductHandler.oExpression = Some(cpHelperInitCodeLines);

				ctx.echo(ctx.enclosingPosition, s"coproduct parser unchecked init for ${show(coproductType)} : ${show(cpHelperInitCodeLines)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");
				ctx.typecheck(cpHelperInitCodeLines.duplicate); // the duplicate is necessary because, according to Dymitro Mitin, the `typeCheck` method mutates its argument sometimes.
				coproductHandler.isCapturingDependencies = false;  // this line must be immediately after the manual type-check
				ctx.echo(ctx.enclosingPosition, s"coproduct parser after init check for ${show(coproductType)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

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
import _root_.jsfacile.macros.CoproductParserMacro.{CpLazy, cpBuffer};
import _root_.jsfacile.macros.ProductParserMacro.{PpLazy, ppBuffer};
import _root_.jsfacile.macros.parsersBufferSemaphore;

if (cpBuffer.size < ${parserHandlersMap.size}) {
	parsersBufferSemaphore.synchronized {
		while(ppBuffer.size < ${parserHandlersMap.size}) {
			ppBuffer.addOne(new PpLazy);
		}
		while(cpBuffer.size < ${parserHandlersMap.size}) {
			cpBuffer.addOne(new CpLazy);
		}
	}
}
{..$inits}
cpBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""

			} else {
				q"""_root_.jsfacile.macros.CoproductParserMacro.cpBuffer(${coproductHandler.typeIndex}).get[$coproductType]"""
			}

		ctx.echo(ctx.enclosingPosition, s"coproduct parser body for ${show(coproductType)}: ${show(body)}\n------\nhandlers:$showParserHandlers\n${showOpenImplicitsAndMacros(ctx)}");

		ctx.Expr[Parser[C]](body);
	}
}



