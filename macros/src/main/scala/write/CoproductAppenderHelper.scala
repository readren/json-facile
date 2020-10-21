package write

import java.util.Comparator

import scala.reflect.macros.blackbox

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

	/** Compares two Strings based on the length and, if both have the same length, by alphabetic order of the reversed names. */
	val productNameComparator: Comparator[String] = { (an, bn) =>
		val anl = an.length;
		var d = anl - bn.length;
		if (d == 0) {
			var i = anl - 1;
			var c: Char = 0;
			do {
				c = an.charAt(i);
				d = c - bn.charAt(i);
				i -= 1;
			} while (d == 0 && i >= 0 && c != '.')
		}
		d
	}

	def materialize[C <: Coproduct]: CoproductAppenderHelper[C] = macro materializeImpl[C];

	def materializeImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[CoproductAppenderHelper[C]] = {
		import ctx.universe._
		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
			val classSymbol = coproductSymbol.asClass;
			val forEachProductSnippet: Seq[ctx.universe.Tree] =
				for {
					productSymbol <- classSymbol.knownDirectSubclasses.toSeq
					productClassSymbol = productSymbol.asClass
					productType = productClassSymbol.toType.dealias
					//	if productType <:< typeOf[Product]
				} yield {
					val productCtorParamsLists = productClassSymbol.primaryConstructor.typeSignatureIn(productType).dealias.paramLists;

					var isFirstField = true;
					val appendFieldsSnippets =
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
val productAppender: Appender[$coproductType] = { (r, p) =>
	r.append('{');
	..${appendFieldsSnippets}
  	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""
				}

			val helper =
				q"""
import _root_.read.CoproductAppenderHelper.CahProductInfo;
import _root_.write.Appender;
import scala.collection.immutable;

val productsInfoBuilder = immutable.Array.newBuilder[CahProductInfo[$coproductType]];

..$forEachProductSnippet

new CoproductParserHelper[$coproductType] {
	override val name = ${coproductSymbol.fullName}
	override val productsInfo = productsInfoBuilder.result();
}"""
			ctx.Expr[CoproductAppenderHelper[C]](ctx.typecheck(helper));
		} else {
			ctx.warning(ctx.enclosingPosition, s"$coproductSymbol should be a sealed trait or abstract class")
			ctx.Expr[CoproductAppenderHelper[C]](q"")
		}
	}

}



