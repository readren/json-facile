package read

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


trait ProductParserHelper[P <: Product] {
	val fullName: String;
	val fieldsInfo: ListMap[String, ProductParserHelper.PphFieldInfo[_]];
	def createProduct(args: Seq[Any]): P
}

object ProductParserHelper {

	/** TODO: make F covariant if [[Parser]] is made covariant. */
	final case class PphFieldInfo[F](valueParser: Parser[F], oDefaultValue: Option[F])

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[P <: Product]: ProductParserHelper[P] = macro materializeHelperImpl[P]

	private val cache: mutable.WeakHashMap[String, blackbox.Context#Tree] = mutable.WeakHashMap.empty

	def materializeHelperImpl[P <: Product : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[ProductParserHelper[P]] = {
		import ctx.universe._

		val productType: Type = ctx.weakTypeTag[P].tpe.dealias;
		val productSymbol: Symbol = productType.typeSymbol;
		if (productSymbol.isClass && !productSymbol.isAbstract) {
			val helper = cache.getOrElseUpdate(
			productSymbol.fullName, {
				val productTypeName: String = show(productType);
			val classSymbol = productSymbol.asClass;
			val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

			val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
			var argIndex = -1;
			val ctorArgumentSnippets =
				for (params <- paramsList) yield {
					for (param <- params) yield {
						argIndex += 1;
						val paramType = param.typeSignature.dealias
						val oDefaultValue =
							if(paramType.typeSymbol.fullName == "scala.Option") {
								q"Some(None)"
							} else {
								q"None"
							}

						addFieldInfoSnippetsBuilder.addOne(
							q"""builder.addOne((${param.name.toString}, PphFieldInfo(Parser.apply[$paramType], $oDefaultValue)));"""
						);
						q"args($argIndex).asInstanceOf[${param.typeSignature}]";
					}
				}
				q"""
import _root_.read.{Parser, ProductParserHelper};
import ProductParserHelper.PphFieldInfo;

val builder = scala.collection.immutable.ListMap.newBuilder[String, PphFieldInfo[_]];
..${addFieldInfoSnippetsBuilder.result()}

new ProductParserHelper[$productType] {
	override val fullName: String = $productTypeName;

	override val fieldsInfo: scala.collection.immutable.ListMap[String, PphFieldInfo[_]] =
		builder.result();

	override def createProduct(args: Seq[Any]):$productType =
		new $productSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
}"""
			}).asInstanceOf[ctx.Tree];

			ctx.Expr[ProductParserHelper[P]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol should be a concrete class")
		}
	}

}
