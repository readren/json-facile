package jsfacile.macros

import java.util.Comparator

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import jsfacile.read.Parser


trait ProductParserHelper[P <: Product] {
	val fullName: String;
	val fieldsInfo: Array[ProductParserHelper.PphFieldInfo[_]];
	def createProduct(args: Seq[Any]): P
}

object ProductParserHelper {

	/** TODO: make F covariant if [[Parser]] is made covariant. */
	final case class PphFieldInfo[F](name: String, valueParser: Parser[F], oDefaultValue: Option[F], ctorArgIndex: Int)

	/** Compares two [[PphFieldInfo]] based solely on their names. */
	val fieldInfoComparator: Comparator[PphFieldInfo[_]] = { (a, b) => a.name.compare(b.name) }

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
							q"""builder.addOne(PphFieldInfo(${param.name.toString}, Parser.apply[$paramType], $oDefaultValue, $argIndex));"""
						);
						q"args($argIndex).asInstanceOf[$paramType]";
					}
				}
				q"""
import _root_.scala.Array;
import _root_.jsfacile.read.Parser;
import _root_.jsfacile.macros.ProductParserHelper;
import ProductParserHelper.{PphFieldInfo, fieldInfoComparator};

val builder = Array.newBuilder[PphFieldInfo[_]];
..${addFieldInfoSnippetsBuilder.result()}

val fieldsArray = builder.result();
_root_.java.util.Arrays.sort(fieldsArray, fieldInfoComparator);


new ProductParserHelper[$productType] {
	override val fullName: String = $productTypeName;

	override val fieldsInfo: Array[PphFieldInfo[_]] = fieldsArray;

	override def createProduct(args: Seq[Any]):$productType = new $productSymbol[..${productType.typeArgs}](...$ctorArgumentSnippets);
}"""
			}).asInstanceOf[ctx.Tree];

//			ctx.info(ctx.enclosingPosition, "pph helper = " + show(helper), false)
			ctx.Expr[ProductParserHelper[P]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$productSymbol should be a concrete class")
		}
	}

}
