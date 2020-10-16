package read

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.reflect.macros.blackbox

import read.CoproductParserHelper._

trait CoproductParserHelper[C <: Coproduct] {
	def discriminator: FieldName;
	def productsInfo: Map[ProductName, ProductInfo[_ <: C]]
	def fieldsInfo: Map[FieldName, Parser[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	type Coproduct = Any;

	case class ProductInfo[+P](fieldsNames: ListMap[FieldName, Option[Any]], constructor: Seq[Any] => P);

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[C <: Coproduct]: CoproductParserHelper[C] = macro materializeHelperImpl[C]

	private val cache: mutable.WeakHashMap[blackbox.Context#Type, blackbox.Context#Expr[CoproductParserHelper[_ <: Coproduct]]] = mutable.WeakHashMap.empty

	def materializeHelperImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[CoproductParserHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.asClass.isSealed) {
//			cache.getOrElseUpdate(coproductType, {
				val classSymbol = coproductSymbol.asClass;
				val forEachProductSnippet: Seq[ctx.universe.Tree] =
					for {
						productSymbol <- classSymbol.knownDirectSubclasses.toSeq
						productClassSymbol = productSymbol.asClass
						productType = productClassSymbol.toType
						if productType <:< typeOf[Product]
					} yield {
						val productCtorParamsLists = productClassSymbol.primaryConstructor.typeSignatureIn(productType).paramLists;

						val forEachFieldSnippet = Seq.newBuilder[ctx.Tree]
						var argIndex = -1;
						val ctorArgumentsTrees =
							for (params <- productCtorParamsLists) yield {
								for (param <- params) yield {
									argIndex += 1;
									forEachFieldSnippet.addOne(
										q"""
			 							fieldsInfoBuilder.addOne(${param.name.toString} -> Parser[${param.typeSignature}])
										productFieldNamesBuilder.addOne(${param.name.toString} -> None);
									   """);
									q"args($argIndex).asInstanceOf[${param.typeSignature}]";
								}
							}
						val ctorFunction = q"(args: Seq[Any]) => new $productSymbol[..${productType.dealias.typeArgs}](...$ctorArgumentsTrees);"

						q"""
		 				..${forEachFieldSnippet.result()}
						productsInfoBuilder.addOne(${productSymbol.name.toString} -> ProductInfo(productFieldNamesBuilder.result(), $ctorFunction));
						productFieldNamesBuilder.clear();
					   """
					}

				val helper =
					q"""
import read.CoproductParserHelper._
import scala.collection.immutable;

val productsInfoBuilder = immutable.Map.newBuilder[ProductName, ProductInfo[_ <: $coproductType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[FieldName, Parser[_]];
val productFieldNamesBuilder = immutable.ListMap.newBuilder[FieldName, Option[Any]];

..$forEachProductSnippet

new CoproductParserHelper[$coproductType] {
	override val discriminator = "PTN";
	override val productsInfo = productsInfoBuilder.result();
	override def fieldsInfo = fieldsInfoBuilder.result();
}"""
				ctx.echo(ctx.enclosingPosition, s"coproductHelper=$helper")

				ctx.Expr[CoproductParserHelper[C]](ctx.typecheck(helper));
//			}).asInstanceOf[ctx.Expr[CoproductParserHelper[C]]]

		} else {
			ctx.warning(ctx.enclosingPosition, s"$coproductSymbol is not a sealed trait and only sealed traits are supported")
			ctx.Expr[CoproductParserHelper[C]](q"")
		}
	}

}



