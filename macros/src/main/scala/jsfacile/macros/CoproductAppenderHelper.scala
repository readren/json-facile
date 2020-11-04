package jsfacile.macros

import java.util.Comparator

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import jsfacile.joint.{Coproduct, ReflectTools, discriminatorField}
import jsfacile.macros.CoproductAppenderHelper.CahProductInfo
import jsfacile.write.Appender

trait CoproductAppenderHelper[C <: Coproduct] {
	def fullName: String;
	def productsInfo: Array[CahProductInfo[C]]
}

object CoproductAppenderHelper {

	case class CahProductInfo[P](name: String, appender: Appender[P])

	/** Compares two [[CahProductInfo]] based their names using the [[productNameComparator]] as ordering criteria.. */
	val productInfoComparator: Comparator[CahProductInfo[_]] = { (a, b) => productNameComparator.compare(a.name, b.name) }

	/** Compares two class names based on the length and, if both have the same length, by alphabetic order of the reversed names.
	 * If the second name (`b`) ends with a dollar, or a dollar followed by digits, they are removed before the comparison begins. This is necessary because the runtime class name of module classes have an extra dollar at the end, local classes have a dollar followed by digits, and local object digits surrounded by dollars.
	 * Differences between dots and dollars are ignored if the dot is in the first name (`a`) and the dollar in the second name (`b`). This is necessary because the runtime class name of nested classes use a dollar instead of a dot to separate the container from members.
	 * The names are considered equal if the fragments after the last dot are equal. */
	val productNameComparator: Comparator[String] = { (a, b) =>
		val aLength = a.length;
		var bLength = b.length;
		var bChar: Char = 0;
		var index: Int = 0;

		// Ignore the last segment of `b` if it matches "(\$\d*)+". This is necessary because the runtime class name of: module classes have an extra dollar at the end, local classes have a dollar followed by a number, and local object have a number surrounded by dollars.
		// Optimized versión
		var continue = false;
		do {
			index = bLength - 1;
			continue = false;
			//  find the index of the last non digit character
			while ( {bChar = b.charAt(index); Character.isDigit(bChar)}) {
				index -= 1;
			}
			// if the index of the last non digit character is a dollar, remove it along with the succeeding digits for the comparison.
			if (b.charAt(index) == '$') {
				bLength = index;
				// if something was removed, repeat the process again to support combinations of edge cases. It is not necessary to know all the edge cases if it's known that any dollar or dollar followed by digits at the end are not part of the original class name. So we can remove combinations of them without fear.
				continue = true;
			}
		} while(continue)

		// here starts the comparison
		var diff = aLength - bLength;
		if (diff == 0 && aLength > 0) {
			index = aLength - 1;
			do {
				val aChar = a.charAt(index);
				bChar = b.charAt(index);
				diff = if (aChar == '.' && bChar == '$') {
					0 // Ignore difference between dots and dollars. This assumes that the first name (an) is obtained by the macro, and the second (bn) may be obtained at runtime from the Class object.
				} else {
					aChar - bChar;
				}
				index -= 1;
			} while (diff == 0 && index >= 0 && bChar != '.')
		}
		diff
	}

	implicit def apply[C <: Coproduct]: CoproductAppenderHelper[C] = macro materializeImpl[C];

	private val cache: mutable.WeakHashMap[whitebox.Context#Type, whitebox.Context#Tree] = mutable.WeakHashMap.empty


	def materializeImpl[C <: Coproduct : ctx.WeakTypeTag](ctx: whitebox.Context): ctx.Expr[CoproductAppenderHelper[C]] = {
		import ctx.universe._

		case class ProductInfo(simpleName: String, tpe: Type, requiredFieldNames: Set[String], appendField_codeLines: List[Tree]) {
			var isAmbiguous = false;
		}

		def addProductsBelongingTo(
			coproductClassSymbol: ClassSymbol,
			coproductType: Type,
			productsInfoBuilder: mutable.Builder[ProductInfo, IndexedSeq[ProductInfo]]
		): Unit = {
			for {
				productSymbol <- coproductClassSymbol.knownDirectSubclasses.toIndexedSeq
			} {
				val productClassSymbol = productSymbol.asClass;
				ReflectTools.applySubclassTypeConstructor[ctx.universe.type](ctx.universe)(coproductType, productClassSymbol.toTypeConstructor) match {
					case Right(productType) =>
						if (productType <:< coproductType) { // this filter filters out the subclasses that are not assignable to the instantiation `C` of the type constructor from where these subclasses extends. This occurs when the subclasses extends the type constructor with different type arguments. Subclasses that are filtered out are ignored and therefore not considered by the ambiguity detector below.

							if (productClassSymbol.isModuleClass) { // if the subclass is a singleton (a scala object), then add a product with no fields
								productsInfoBuilder.addOne(ProductInfo(
									productClassSymbol.name.toString,
									productClassSymbol.toType,
									Set.empty,
									Nil
								))

							} else if (productSymbol.isAbstract) { // if the subclass is abstract (a scala abstract class or trait), then call `addProductsBelongingTo` recursively
								if( productClassSymbol.isSealed) {
									addProductsBelongingTo(productClassSymbol, productType, productsInfoBuilder)
								} else {
									ctx.abort(ctx.enclosingPosition, s"$productClassSymbol should be sealed")
								}

							} else { // if the subclass is a concrete non singleton class (a scala class), then add a product whose fields are the parameters of said subclass primary constructor.
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
										val sb = new StringBuilder(paramNameStr.length + 4)
										if (isFirstField) {
											isFirstField = false
										} else {
											sb.append(',');
										}
										sb.append('"').append(paramNameStr).append('"').append(':');
										q"""r.append(${sb.toString}).appendSummoned[$paramTypeSignature](${Select(Ident(TermName("p")), param.name)})"""; // IntellijIde reports false error here
									}

								productsInfoBuilder.addOne(ProductInfo(
									productSymbol.name.toString,
									productType,
									requiredFieldNamesBuilder.result(),
									appendField_codeLines
								))
							}

						}

					case Left(freeTypeParams) =>
						ctx.abort(ctx.enclosingPosition, s"""The "$productSymbol", which is a subclass of "${coproductClassSymbol.fullName}", has at least one free type parameters (it does not depend on the supertype and, therefore, there is no way to determine its actual type knowing only the super type). The free type parameters are: ${freeTypeParams.mkString}.""")
				}
			}
		}

		val coproductType: Type = ctx.weakTypeTag[C].tpe.dealias;
		val coproductSymbol: Symbol = coproductType.typeSymbol;
		if (coproductSymbol.isClass && coproductSymbol.isAbstract && coproductSymbol.asClass.isSealed) {
			val helper = cache.getOrElseUpdate(
			coproductType, {
				val coproductClassSymbol = coproductSymbol.asClass;

				// Get the discriminator field name and requirement from the coproduct annotation, or the default values if it isn't annotated.
				val (discriminatorFieldName, discriminatorIsRequired) = discriminatorField.parse(ctx.universe)(coproductClassSymbol)

				val productsInfoBuilder = IndexedSeq.newBuilder[ProductInfo]
				addProductsBelongingTo(coproductClassSymbol, coproductType, productsInfoBuilder);
				val productsInfo = productsInfoBuilder.result();

				// Set the `isAmbiguous` flag to every product whose required field names match those of another product.
				if (!discriminatorIsRequired) {
					var i = productsInfo.size - 1;
					while (i > 0) {
						val pi = productsInfo(i);
						if (!pi.isAmbiguous) {
							var j = i - 1;
							while (j >= 0) {
								val pj = productsInfo(j);
								if (pi.requiredFieldNames == pj.requiredFieldNames) {
									pi.isAmbiguous = true;
									pj.isAmbiguous = true;
								}
								j -= 1;
							}
						}
						i -= 1
					}
				}

				// for every product, generate the code lines that creates the [[CahProductInfo]] and adds it to the `productsInfoBuilder`
				val addProductInfo_codeLines: Seq[ctx.universe.Tree] =
					for {
						productInfo <- productsInfo
					} yield {
						val appendDiscriminator_codeLine =
							if (discriminatorIsRequired || productInfo.isAmbiguous) {
								val discriminatorField = s""""$discriminatorFieldName":"${productInfo.simpleName}"${if (productInfo.appendField_codeLines.isEmpty) "" else ","}"""
								q"r.append($discriminatorField)"
							} else {
								q""
							}
						val productClassNameAtRuntime = productInfo.tpe.erasure.typeSymbol.fullName;
						q"""
val productAppender: _root_.jsfacile.write.Appender[${productInfo.tpe}] = { (r, p) =>
	r.append('{')

 	$appendDiscriminator_codeLine

	..${productInfo.appendField_codeLines}

  	r.append('}')
}
productsInfoBuilder.addOne(CahProductInfo($productClassNameAtRuntime, productAppender));"""
					}

				q"""
import _root_.scala.Array;
import _root_.jsfacile.macros.CoproductAppenderHelper;
import CoproductAppenderHelper.CahProductInfo;

val productsInfoBuilder = Array.newBuilder[CahProductInfo[_ <: $coproductType]];

..$addProductInfo_codeLines

val productsArray = productsInfoBuilder.result().asInstanceOf[Array[CahProductInfo[$coproductType]]];
_root_.java.util.Arrays.sort(productsArray, CoproductAppenderHelper.productInfoComparator);

new CoproductAppenderHelper[$coproductType] {
	override val fullName = ${coproductSymbol.fullName}
	override val productsInfo = productsArray;
}"""
			}).asInstanceOf[ctx.Tree];
			// ctx.info(ctx.enclosingPosition, show(helper), false)

			ctx.Expr[CoproductAppenderHelper[C]](ctx.typecheck(helper));
		} else {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a sealed trait or abstract class")
		}
	}
}



