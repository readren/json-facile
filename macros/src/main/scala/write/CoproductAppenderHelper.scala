package write

import java.util.Comparator

import scala.reflect.macros.whitebox

import read.CoproductParserHelper.Coproduct
import write.CoproductAppenderHelper.CahProductInfo

trait CoproductAppenderHelper[C <: Coproduct] {
	def name: String;
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
				if(ac == '.' && bc == '$') {
					d = 0
				}
				i -= 1;
			} while (d == 0 && i >= 0 && bc != '.')
		}
		d
	}

	implicit def apply[C <: Coproduct]: CoproductAppenderHelper[C] = macro materializeImpl[C];

	def materializeImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductAppenderHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
			val classSymbol = coproductSymbol.asClass;
			val addProductInfo_codeLines: Seq[ctx.universe.Tree] =
				for {
					productSymbol <- classSymbol.knownDirectSubclasses.toSeq
				} yield {
					val productType = util.ReflectTools.applySubclassTypeConstructor(ctx.universe)(coproductType, productSymbol.asClass.toTypeConstructor)
					val productCtorParamsLists = productType.typeSymbol.asClass.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

					var isFirstField = true;
					val appendField_codeLines =
						for {
							params <- productCtorParamsLists
							param <- params
						} yield {
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
	.append(${param.name.toString})
	.append("\":")
	.appendSummoned[${param.typeSignature.dealias}](${Select(Ident(TermName("p")), param.name)})""";
						}

					val productClassNameAtRuntime = productType.erasure.typeSymbol.fullName;
					q"""
val productAppender: _root_.write.Appender[$productType] = { (r, p) =>
	r.append('{')

	..$appendField_codeLines

  	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""
				}

			val helper =
				q"""
import _root_.write.api._
import _root_.write.CoproductAppenderHelper;
import CoproductAppenderHelper.CahProductInfo;

val productsInfoBuilder = scala.Array.newBuilder[CahProductInfo[_ <: $coproductType]];

..$addProductInfo_codeLines

val productsArray = productsInfoBuilder.result().asInstanceOf[Array[CahProductInfo[$coproductType]]];
java.util.Arrays.sort(productsArray, CoproductAppenderHelper.productInfoComparator);

new CoproductAppenderHelper[$coproductType] {
	override val name = ${coproductSymbol.fullName}
	override val productsInfo = productsArray;

}"""
			ctx.Expr[CoproductAppenderHelper[C]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol should be a sealed trait or abstract class")
		}
	}
}



