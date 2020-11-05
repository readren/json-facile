package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.{CoproductUpperBound, ReflectTools, discriminatorField}
import jsfacile.macros.CoproductParserHelper.{CphProductInfo, FieldName}
import jsfacile.read.Parser

trait CoproductParserHelper[C <: CoproductUpperBound] {
	def fullName: String;
	def discriminator: FieldName;
	def productsInfo: ArraySeq[CphProductInfo[_ <: C]]
	def fieldsInfo: Map[FieldName, Parser[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	final case class CphFieldInfo[+V](name: FieldName, oDefaultValue: Option[V])
	final case class CphProductInfo[+P](name: ProductName, numberOfRequiredFields: Int, fields: Seq[CphFieldInfo[Any]], constructor: Seq[Any] => P);

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[C <: CoproductUpperBound]: CoproductParserHelper[C] = macro materializeHelperImpl[C]

	private val cache: mutable.WeakHashMap[whitebox.Context#Type, whitebox.Context#Tree] = mutable.WeakHashMap.empty

	/** Traits for which the [[jsfacile.read]] package provides an implicit [[Parser]]. */
	val traitsForWhichTheReadPackageProvidesAnImplicitParser: Set[String] = Set(
		classOf[scala.Option[Any]].getName, // by jpOption
		classOf[scala.collection.Iterable[Any]].getName, // by jpIterable
		classOf[scala.collection.Map[Any, Any]].getName, // by jpUnsortedMap
		classOf[scala.collection.SortedMap[_, Any]].getName // by jpSortedMap
	);

	def materializeHelperImpl[C <: CoproductUpperBound : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._

		/** Tell for which types is a [[Parser]] already provided in the [[jsfacile.read]] package, in order to avid letting the [[jsfacile.read.jpCoproduct]] generate another and cause ambiguity error. */
		def doesTheReadPackageProvideAnImplicitParserFor(classSymbol: ClassSymbol): Boolean = {
			classSymbol.baseClasses.exists(bc => traitsForWhichTheReadPackageProvidesAnImplicitParser.contains(bc.fullName))
		}

		/** Adds to the `productsSnippetBuilder` a snippet for each product that extends the specified trait (or abstract class). */
		def addProductsBelongingTo(
			coproductClassSymbol: ClassSymbol,
			coproductType: Type,
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
	immutable.ArraySeq.empty[CphFieldInfo[Any]],
	(args: Seq[Any]) => ${productClassSymbol.module}
));"""
								)

							} else if (productClassSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
								if (productClassSymbol.isSealed) {
									addProductsBelongingTo(productClassSymbol, productType, productsSnippetsBuilder)
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
											argIndex += 1;
											val oDefaultValue =
												if (paramType.typeSymbol.fullName == "scala.Option") {
													q"Some(None)"
												} else {
													requiredFieldsCounter += 1;
													q"None"
												}
											forEachFieldSnippet.addOne(
												q"""
fieldsInfoBuilder.addOne((${param.name.toString}, Parser[$paramType]));
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
		val helper = cache.getOrElseUpdate(
		coproductType, {
			// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
			val discriminatorFieldName: String = discriminatorField.parse(ctx.universe)(coproductClassSymbol)._1;

			val productsSnippetsBuilder = Seq.newBuilder[ctx.universe.Tree];
			addProductsBelongingTo(coproductClassSymbol, coproductType, productsSnippetsBuilder);

			q"""
import _root_.scala.collection.immutable;
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.CoproductParserHelper;
import CoproductParserHelper.{CphProductInfo, CphFieldInfo}

val productsInfoBuilder = immutable.ArraySeq.newBuilder[CphProductInfo[_ <: $coproductType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[String, Parser[_]];
val productFieldsSeqBuilder = immutable.ArraySeq.newBuilder[CphFieldInfo[Any]];

..${productsSnippetsBuilder.result()}

new CoproductParserHelper[$coproductType] {
	override val fullName = ${coproductType.toString}
	override val discriminator = $discriminatorFieldName;
	override val productsInfo = productsInfoBuilder.result();
	override val fieldsInfo = fieldsInfoBuilder.result();
}"""
		}).asInstanceOf[ctx.Tree];
		// ctx.info(ctx.enclosingPosition, "cph helper = " + show(helper), false)

		ctx.Expr[CoproductParserHelper[C]](ctx.typecheck(helper));
	}
}



