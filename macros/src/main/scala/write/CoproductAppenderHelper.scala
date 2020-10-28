package write

import java.util.Comparator

import scala.collection.mutable
import scala.reflect.macros.whitebox

import read.CoproductParserHelper
import read.CoproductParserHelper.Coproduct
import write.CoproductAppenderHelper.CahProductInfo

trait CoproductAppenderHelper[C <: Coproduct] {
	def fullName: String;
	def productsInfo: Array[CahProductInfo[C]]
}

object CoproductAppenderHelper {

	case class CahProductInfo[P](name: String, appender: Appender[P])

	/** Compares two [[CahProductInfo]] based their names using the [[productNameComparator]] as ordering criteria.. */
	val productInfoComparator: Comparator[CahProductInfo[_]] = { (a, b) => productNameComparator.compare(a.name, b.name) }

	/** Compares two Strings based on the length and, if both have the same length, by alphabetic order of the reversed names.
	 * Differences between dots and dollars are ignored if the dot is in the first name (an) and the dollar in the second name (bn).
	 * The names are considered equal if the fragments after the last dot are equal. */
	val productNameComparator: Comparator[String] = { (an, bn) =>
		val anl = an.length;
		var d = anl - bn.length;
		if (d == 0) {
			var i = anl - 1;
			var bc: Char = 0;
			do {
				val ac = an.charAt(i);
				bc = bn.charAt(i);
				d = ac - bc;
				// Ignore difference between dots and dollars. This assumes that the first name (an) is obtained by the macro, and the second (bn) may be obtained at runtime from the Class object.
				if (ac == '.' && bc == '$') {
					d = 0
				}
				i -= 1;
			} while (d == 0 && i >= 0 && bc != '.')
		}
		d
	}

	implicit def apply[C <: Coproduct]: CoproductAppenderHelper[C] = macro materializeImpl[C];

	private val cache: mutable.WeakHashMap[String, whitebox.Context#Tree] = mutable.WeakHashMap.empty

	def materializeImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductAppenderHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
			val helper = cache.getOrElseUpdate(
			coproductSymbol.fullName, {
				val classSymbol = coproductSymbol.asClass;

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val (discriminatorFieldName, discriminatorIsRequired) = joint.discriminatorField.parse(ctx.universe)(classSymbol)

				case class ProductInfo(simpleName: String, tpe: ctx.Type, requiredFieldNames: Set[String], appendField_codeLines: List[Tree]) {
					var isAmbiguous = false;
				}

				val productsInfoBuilder = IndexedSeq.newBuilder[ProductInfo]
				for {
					productSymbol <- classSymbol.knownDirectSubclasses.toIndexedSeq
				} {
					val productType = util.ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productSymbol.asClass.toTypeConstructor)
					val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

					val requiredFieldNamesBuilder = Set.newBuilder[String];
					var isFirstField = true;
					val appendField_codeLines =
						for {
							params <- productCtorParamsLists
							param <- params
						} yield {
							val paramNameStr = param.name.decodedName.toString;
							val paramTypeSignature = param.typeSignature.dealias;
							if (paramTypeSignature.typeSymbol.fullName != "scala.Option") {
								requiredFieldNamesBuilder.addOne(paramNameStr)
							}
							val start = {
								if (isFirstField) {
									isFirstField = false;
									q"""r.append('"')"""
								} else {
									q"""r.append(",\"")"""
								}
							}
							q"""
$start
	.append($paramNameStr)
	.append("\":")
	.appendSummoned[$paramTypeSignature](${Select(Ident(TermName("p")), param.name)})"""; // IntellijIde reports false error here
						}

					productsInfoBuilder.addOne(ProductInfo(
						productSymbol.name.toString,
						productType,
						requiredFieldNamesBuilder.result(),
						appendField_codeLines
					))
				}
				val productsInfo = productsInfoBuilder.result();

				// Set the `isAmbiguous` flag to every product whose required field names match those of another product.
				if (!discriminatorIsRequired) {
					var i = productsInfo.size - 1;
					var pi = productsInfo(i);
					while (i > 0) {
						var j = i - 1;
						while (j >= 0) {
							val pj = productsInfo(j);
							if (pi.requiredFieldNames == pj.requiredFieldNames) {
								pi.isAmbiguous = true;
								pj.isAmbiguous = true;
							}
							j -= 1;
						}
						do {
							i -= 1
							pi = productsInfo(i);
						} while (productsInfo(i).isAmbiguous)
					}
				}

				// for every product, generate the code lines creates the [[CahProductInfo]] and adds it to the `productsInfoBuilder`
				val addProductInfo_codeLines: Seq[ctx.universe.Tree] =
					for {
						productInfo <- productsInfo
					} yield {
						val appendDiscriminator_codeLine =
							if (discriminatorIsRequired || productInfo.isAmbiguous) {
								val discriminatorField = s""""$discriminatorFieldName":"${productInfo.simpleName}"${if (productsInfo.isEmpty) "" else ","}"""
								q"r.append($discriminatorField)"
							} else {
								q""
							}
						val productClassNameAtRuntime = productInfo.tpe.erasure.typeSymbol.fullName;
						q"""
val productAppender: _root_.write.Appender[${productInfo.tpe}] = { (r, p) =>
	r.append('{')

 	$appendDiscriminator_codeLine

	..${productInfo.appendField_codeLines}

  	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""
					}

				q"""
import _root_.write.api._
import _root_.write.CoproductAppenderHelper;
import CoproductAppenderHelper.CahProductInfo;

val productsInfoBuilder = scala.Array.newBuilder[CahProductInfo[_ <: $coproductType]];

..$addProductInfo_codeLines

val productsArray = productsInfoBuilder.result().asInstanceOf[Array[CahProductInfo[$coproductType]]];
java.util.Arrays.sort(productsArray, CoproductAppenderHelper.productInfoComparator);

new CoproductAppenderHelper[$coproductType] {
	override val fullName = ${coproductSymbol.fullName}
	override val productsInfo = productsArray;
}"""
			}).asInstanceOf[ctx.Tree];

			ctx.Expr[CoproductAppenderHelper[C]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol should be a sealed trait or abstract class")
		}
	}
}



