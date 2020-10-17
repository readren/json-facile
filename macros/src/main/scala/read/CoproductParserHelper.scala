package read

import scala.collection.immutable.{ArraySeq, ListMap}
import scala.collection.mutable
import scala.reflect.macros.blackbox

import read.CoproductParserHelper._

trait CoproductParserHelper[C <: Coproduct] {
	def name: String;
	def discriminator: FieldName;
	def productsInfo: ArraySeq[ProductInfo[_ <: C]]
	def fieldsInfo: Map[FieldName, Parser[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	type Coproduct = Any;

	case class CFieldInfo[+V](name: FieldName, oDefaultValue: Option[V])
	case class ProductInfo[+P](name: ProductName, numberOfRequiredFields: Int, fields: Seq[CFieldInfo[Any]], constructor: Seq[Any] => P);

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[C <: Coproduct]: CoproductParserHelper[C] = macro materializeHelperImpl[C]

	private val cache: mutable.WeakHashMap[blackbox.Context#Type, blackbox.Context#Expr[CoproductParserHelper[_ <: Coproduct]]] = mutable.WeakHashMap.empty

	def materializeHelperImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.asClass.isSealed) {
//			cache.getOrElseUpdate(coproductType, {
				val classSymbol = coproductSymbol.asClass;
				val forEachProductSnippet: Seq[ctx.universe.Tree] =
					for {
						productSymbol <- classSymbol.knownDirectSubclasses.toSeq
						productClassSymbol = productSymbol.asClass
						productType = productClassSymbol.toType.dealias
						if productType <:< typeOf[Product]
					} yield {
						val productCtorParamsLists = productClassSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

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
			 							fieldsInfoBuilder.addOne(${param.name.toString} -> Parser[${paramType}])
										productFieldsSeqBuilder.addOne(CFieldInfo(${param.name.toString}, None));
									   """);
									q"args($argIndex).asInstanceOf[${paramType}]";
								}
							}
						val ctorFunction = q"(args: Seq[Any]) => new $productSymbol[..${productType.typeArgs}](...$ctorArgumentsTrees);"

						q"""
		 				..${forEachFieldSnippet.result()}
						productsInfoBuilder.addOne(ProductInfo(${productSymbol.name.toString}, $requiredFieldsCounter, productFieldsSeqBuilder.result(), $ctorFunction));
						productFieldsSeqBuilder.clear();
					   """
					}

				val helper =
					q"""
import read.CoproductParserHelper.{ProductInfo, CFieldInfo, FieldName}
import scala.collection.immutable;

val productsInfoBuilder = immutable.ArraySeq.newBuilder[ProductInfo[_ <: $coproductType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[FieldName, Parser[_]];
val productFieldsSeqBuilder = immutable.ArraySeq.newBuilder[CFieldInfo[Any]];

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
			ctx.warning(ctx.enclosingPosition, s"$coproductSymbol is not a sealed trait and only sealed traits are supported")
			ctx.Expr[CoproductParserHelper[C]](q"")
		}
	}

}



