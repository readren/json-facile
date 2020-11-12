package jsfacile.macros

import java.util.Comparator

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import jsfacile.joint.ProductUpperBound
import jsfacile.read.Parser


trait ProductParserHelper[P <: ProductUpperBound] {
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
	implicit def materializeHelper[P <: ProductUpperBound]: ProductParserHelper[P] = macro materializeHelperImpl[P]

	private val cache: mutable.HashMap[blackbox.Context#Type, blackbox.Context#Tree] = mutable.HashMap.empty

	/** Concrete classes for which the [[jsfacile.read]] package provides an implicit [[Parser]]. */
	val concreteClassesForWhichTheReadPackageProvidesAnImplicitParser: Set[String] = Set(
		classOf[java.lang.String].getName, // by jpString
		classOf[scala.math.BigDecimal].getName, // by jpBigDecimal
		classOf[scala.math.BigInt].getName, // by jpBigInt
		classOf[scala.Some[Any]].getName // by jpSome
	);

	def materializeHelperImpl[P <: ProductUpperBound : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[ProductParserHelper[P]] = {
		import ctx.universe._

		/** Used by the [[CoproductParserHelper.materializeHelper]] macro to avoid generating parsers for types that are already provided by this package. */
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

		ctx.info(ctx.enclosingPosition, s"product parser helper start of ${show(productType)}", false)

		val helper = cache.getOrElseUpdate(
		productType, {
			val productTypeName: String = show(productType);
			val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

			val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
			var argIndex = -1;
			val ctorArgumentSnippets =
				for (params <- paramsList) yield {
					for (param <- params) yield {
						argIndex += 1;
						val paramType = param.typeSignature.dealias
						val oDefaultValue =
							if (paramType.typeSymbol.fullName == "scala.Option") {
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
		ctx.info(ctx.enclosingPosition, s"product parser helper end of ${show(productType)}: ${show(helper)}", false)

		ctx.Expr[ProductParserHelper[P]](ctx.typecheck(helper));
	}
}
