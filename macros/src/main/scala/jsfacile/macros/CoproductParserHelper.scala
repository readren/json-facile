package jsfacile.macros

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.{Coproduct, ReflectTools, discriminatorField}
import jsfacile.macros.CoproductParserHelper.{CphProductInfo, FieldName}
import jsfacile.read.Parser

trait CoproductParserHelper[C <: Coproduct] {
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
	implicit def materializeHelper[C <: Coproduct]: CoproductParserHelper[C] = macro materializeHelperImpl[C]

	private val cache: mutable.WeakHashMap[whitebox.Context#Type, whitebox.Context#Tree] = mutable.WeakHashMap.empty

	def materializeHelperImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
			val helper = cache.getOrElseUpdate(
			coproductType, {
				val classSymbol = coproductSymbol.asClass;
				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val discriminatorFieldName: String = discriminatorField.parse(ctx.universe)(classSymbol)._1;

				val forEachProductSnippet: Seq[ctx.universe.Tree] =
					for (productSymbol <- classSymbol.knownDirectSubclasses.toSeq) yield {

						if (productSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields nor constructor.
							q"""
productsInfoBuilder.addOne(CphProductInfo(
	${productSymbol.name.toString},
	0,
	immutable.ArraySeq.empty[CphFieldInfo[Any]],
	(args: Seq[Any]) => ${productSymbol.asClass.module}
));"""

						} else if(productSymbol.isAbstract ) { // if the subclass is abstract (a scala abstract class or trait), then // TODO support nested traits (make this a recursive loop)
							ctx.abort(ctx.enclosingPosition, "Nested traits are not supported yet")
							q""

						} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose constructor is its primary one, and its fields are its primary constructor parameters.
							val productType = ReflectTools.applySubclassTypeConstructor(ctx.universe)(coproductType, productSymbol.asClass.toTypeConstructor);
							// TODO hacer que soporte case objects.
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
							val ctorFunction = q"(args: Seq[Any]) => new $productSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

							q"""
..${forEachFieldSnippet.result()}
productsInfoBuilder.addOne(CphProductInfo(${productSymbol.name.toString}, $requiredFieldsCounter, productFieldsSeqBuilder.result(), $ctorFunction));
productFieldsSeqBuilder.clear();"""

						}
					}
				q"""
import _root_.scala.collection.immutable;
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.CoproductParserHelper;
import CoproductParserHelper.{CphProductInfo, CphFieldInfo}

val productsInfoBuilder = immutable.ArraySeq.newBuilder[CphProductInfo[_ <: $coproductType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[String, Parser[_]];
val productFieldsSeqBuilder = immutable.ArraySeq.newBuilder[CphFieldInfo[Any]];

..$forEachProductSnippet

new CoproductParserHelper[$coproductType] {
	override val fullName = ${coproductSymbol.fullName}
	override val discriminator = $discriminatorFieldName;
	override val productsInfo = productsInfoBuilder.result();
	override val fieldsInfo = fieldsInfoBuilder.result();
}"""
			}).asInstanceOf[ctx.Tree];
			// ctx.info(ctx.enclosingPosition, "cph helper = " + show(helper), false)

			ctx.Expr[CoproductParserHelper[C]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol should be a sealed trait or abstract class")
		}
	}
}



