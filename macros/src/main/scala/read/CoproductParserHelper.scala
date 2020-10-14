package read

import scala.collection.immutable.ListMap
import scala.reflect.macros.blackbox

import read.CoproductParserHelper._

trait CoproductParserHelper[C <: Coproduct] {
	def discriminator: FieldName;
	def productsInfo: Map[ProductName, ProductInfo[_ <: C]]
	def fieldsInfo: Map[FieldName, FieldInfo[_]];
}

object CoproductParserHelper {
	type ProductName = String;
	type FieldName = String;

	trait Coproduct;

	/** TODO: make F covariant when the compiler's implicit search bug is solved  */
	case class FieldInfo[F](parser: Parser[F])
	case class ProductInfo[+P](fieldsNames: ListMap[FieldName, Option[Any]], constructor: Seq[Any] => P);


	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[C <: Coproduct]: CoproductParserHelper[C] = macro materializeHelperImpl[C]


	def materializeHelperImpl[C <: Coproduct : c.WeakTypeTag](c: blackbox.Context): c.Expr[CoproductParserHelper[C]] = {
		import c.universe._
		val tWtt: WeakTypeTag[C] = c.weakTypeTag[C];
		val cType: Type = tWtt.tpe;
		val cSymbol: Symbol = cType.typeSymbol;
		if (cSymbol.isClass && cSymbol.asClass.isSealed) {
			val classSymbol = cSymbol.asClass;
			val forEachProductSnippet: Seq[c.universe.Tree] =
				for {
					productSymbol <- classSymbol.knownDirectSubclasses.toSeq
					productClassSymbol = productSymbol.asClass
					productType = productClassSymbol.toType
					if productType <:< typeOf[Product]
				} yield {
					val productCtorParamsLists = productClassSymbol.primaryConstructor.typeSignatureIn(productType).paramLists;

					val forEachFieldSnippet = Seq.newBuilder[c.Tree]
					var argIndex = 0;
					val ctorArgumentsTrees =
						for (params <- productCtorParamsLists) yield {
							for (param <- params) yield {
								forEachFieldSnippet.addOne(
									q"""
			 							fieldsInfoBuilder.addOne(${param.name.toString} -> FieldInfo(Parser[${param.typeSignature}]))
										productFieldNamesBuilder.addOne(${param.name.toString} -> None);
									   """);
								val argTree = q"args($argIndex).asInstanceOf[${param.typeSignature}]";
								argIndex += 1;
								argTree
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

val productsInfoBuilder = immutable.Map.newBuilder[ProductName, ProductInfo[_ <: $cType]];
val fieldsInfoBuilder = immutable.Map.newBuilder[FieldName, FieldInfo[_]];
val productFieldNamesBuilder = immutable.ListMap.newBuilder[FieldName, Option[Any]];

..$forEachProductSnippet

new CoproductParserHelper[$cType] {
	override val discriminator = "PTN";
	override val productsInfo = productsInfoBuilder.result();
	override def fieldsInfo = fieldsInfoBuilder.result();
}"""
			c.echo(c.enclosingPosition, s"coproductHelper=$helper")

			c.Expr[CoproductParserHelper[C]](helper)

		} else {
			c.warning(c.enclosingPosition, s"$cSymbol is not a sealed trait and only sealed traits are supported")
			c.Expr[CoproductParserHelper[C]](q"")
		}
	}

}



