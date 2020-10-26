package read

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.macros.whitebox

import read.CoproductParserHelper._

trait CoproductParserHelper[C <: Coproduct] {
	def name: String;
	def discriminator: FieldName;
	def productsInfo: ArraySeq[CphProductInfo[_ <: C]]
	def fieldsInfo: Map[FieldName, Parser[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	type Coproduct = Any;

	final case class CphFieldInfo[+V](name: FieldName, oDefaultValue: Option[V])
	final case class CphProductInfo[+P](name: ProductName, numberOfRequiredFields: Int, fields: Seq[CphFieldInfo[Any]], constructor: Seq[Any] => P);

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[C <: Coproduct]: CoproductParserHelper[C] = macro materializeHelperImpl[C]

//	private val cache: mutable.WeakHashMap[whitebox.Context#Type, whitebox.Context#Expr[CoproductParserHelper[_ <: Coproduct]]] = mutable.WeakHashMap.empty

	def materializeHelperImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
//			cache.getOrElseUpdate(coproductType, {
				val classSymbol = coproductSymbol.asClass;
				val forEachProductSnippet: Seq[ctx.universe.Tree] =
					for (productSymbol <- classSymbol.knownDirectSubclasses.toSeq) yield {
						val productType = util.ReflectTools.applySubclassTypeConstructor(ctx.universe)(coproductType, productSymbol.asClass.toTypeConstructor)
						val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).paramLists

						val forEachFieldSnippet = Seq.newBuilder[ctx.Tree];
						var requiredFieldsCounter: Int = 0;
						var argIndex = -1;
						val ctorArgumentsTrees =
							for (params <- productCtorParamsLists) yield {
								for (param <- params) yield {
									val paramType = param.typeSignature.dealias
									argIndex += 1;
									requiredFieldsCounter += 1; // TODO this will change when default field values are fetched.
									forEachFieldSnippet.addOne(
										q"""
			 							fieldsInfoBuilder.addOne(${param.name.toString} -> Parser[$paramType])
										productFieldsSeqBuilder.addOne(CphFieldInfo(${param.name.toString}, None));
									   """);
									q"args($argIndex).asInstanceOf[$paramType]";
								}
							}
						val ctorFunction = q"(args: Seq[Any]) => new $productSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

						q"""
		 				..${forEachFieldSnippet.result()}
						productsInfoBuilder.addOne(CphProductInfo(${productSymbol.name.toString}, $requiredFieldsCounter, productFieldsSeqBuilder.result(), $ctorFunction));
						productFieldsSeqBuilder.clear();
					   """
					}

				val helper =
					q"""
import scala.collection.immutable;
import _root_.read.{Parser, CoproductParserHelper};
import CoproductParserHelper.{CphProductInfo, CphFieldInfo};
import _root_.read.api._

val productsInfoBuilder = immutable.ArraySeq.newBuilder[CphProductInfo[_ <: $coproductType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[String, Parser[_]];
val productFieldsSeqBuilder = immutable.ArraySeq.newBuilder[CphFieldInfo[Any]];

..$forEachProductSnippet

new CoproductParserHelper[$coproductType] {
	override val name = ${coproductSymbol.fullName}
	override val discriminator = "PTN";
	override val productsInfo = productsInfoBuilder.result();
	override val fieldsInfo = fieldsInfoBuilder.result();
}"""
				ctx.Expr[CoproductParserHelper[C]](ctx.typecheck(helper));
//			}).asInstanceOf[ctx.Expr[CoproductParserHelper[C]]]

		} else {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol should be a sealed trait or abstract class")
		}
	}

}



