package read

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


trait ProductParserHelper[P <: Product] {
	val className: String;
	val fieldsInfo: ListMap[String, ProductParserHelper.FieldInfo[_]];
	def createProduct(args: Seq[Any]): P
}

object ProductParserHelper {

	/** TODO: make F covariant when the compiler's implicit search bug is solved  */
	case class FieldInfo[F](valueParser: Parser[F], oDefaultValue: Option[F])

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[P <: Product]: ProductParserHelper[P] = macro materializeHelperImpl[P]

	/** Ejemplo del código que genera este macro si fuera invocado así: {{{
	 *	case class Person[W](name: String, age: Int, work: W);
	 *	materializeGuia[Person[Double]];
	 * }}}
	 * Resultado:
	 * {{{
	 *	new GuiaLectorProducto[Person[Double]] {
	 *		override val className: String = className;
	 *
	 *		override val infoCampos: ListMap[String, InfoCampo[Any]] = {
	 *
	 *			val builder = ListMap.newBuilder[String, InfoCampo[Any]];
	 *
	 *			val name = Interpretador.apply[String];
	 *			builder.addOne(("name", InfoCampo(name, None)));
	 *			val age = Interpretador.apply[Int];
	 *			builder.addOne(("age", InfoCampo(age, None)));
	 *			val work = Interpretador.apply[Double];
	 *			builder.addOne(("campoN", InfoCampo(work, None)));
	 *
	 *			builder.result();
	 *		}
	 *
	 *		override def crear(args: Seq[Any]): Person[Double] = {
	 *			new Person[Double](args(0).asInstanceOf[String], args(1).asInstanceOf[Int], args(2).asInstanceOf[Double]);
	 *		}
	 *	}
	 * }}}
	 */
	def materializeHelperImpl[P <: Product : c.WeakTypeTag](c: blackbox.Context): c.Expr[ProductParserHelper[P]] = {
		import c.universe._
		val pWtt: WeakTypeTag[P] = c.weakTypeTag[P];
		val productType: Type = pWtt.tpe;
		val productSymbol: Symbol = productType.typeSymbol;
		if (productSymbol.isClass) {
			val productTypeName: String = show(productType);
			val classSymbol = productSymbol.asClass;
			val paramsList = classSymbol.primaryConstructor.typeSignatureIn(productType).paramLists;

			val addFieldInfoSnippetsBuilder = List.newBuilder[Tree];
			var argIndex = 0;
			val ctorArgumentSnippets =
				for (params <- paramsList) yield {
					for (param <- params) yield {
						addFieldInfoSnippetsBuilder.addOne(q"""builder.addOne((${param.name.toString}, read.ProductParserHelper.FieldInfo(Parser.apply[${param.typeSignature}], None)));""")
						val argTree = q"args($argIndex).asInstanceOf[${param.typeSignature}]";
						argIndex += 1;
						argTree
					}
				}
			val helper =
				q"""
import read.ProductParserHelper.FieldInfo;

val builder = scala.collection.immutable.ListMap.newBuilder[String, FieldInfo[_]];
..${addFieldInfoSnippetsBuilder.result()}

new ProductParserHelper[$productType] {
	override val className: String = $productTypeName;

	override val fieldsInfo: scala.collection.immutable.ListMap[String, FieldInfo[_]] =
		builder.result();

	override def createProduct(args: Seq[Any]):$productType =
		new ${productType.typeSymbol}[..${productType.dealias.typeArgs}](...${ctorArgumentSnippets});
}""";
			c.echo(c.enclosingPosition, s"productHelper=$helper")

			c.Expr[ProductParserHelper[P]](helper)

		} else {
			c.warning(c.enclosingPosition, s"$productSymbol is not a class and only classes are supported")
			c.Expr[ProductParserHelper[P]](q"")
		}
	}

}
