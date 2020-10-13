package read

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


trait ProductParserHelper[T <: Product] {
	val className: String;
	val fieldsInfo: ListMap[String, ProductParserHelper.FieldInfo[_]];
	def createProduct(args: Seq[Any]): T
}

object ProductParserHelper {

	case class FieldInfo[C](valueParser: Parser[C], oDefaultValue: Option[C])

	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeHelper[T <: Product]: ProductParserHelper[T] = macro materializeHelperImpl[T]

	/** Ejemplo del código que genera este macro si fuera invocado así: {{{
	 *	case class Person[W](name: String, age: Int, work: W);
	 *	materializeGuia[Person[Double]];
	 * }}}
	 * Resultado:
	 * {{{
	 *	new GuiaLectorProducto[Person[Double]] {
	 *		override val className: String = className;
	 *
	 *		override val infoCampos: ListMap[String, InfoCampo[_]] = {
	 *
	 *			val builder = ListMap.newBuilder[String, InfoCampo[_ <: Any]];
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
	def materializeHelperImpl[T <: Product : c.WeakTypeTag](c: blackbox.Context): c.Expr[ProductParserHelper[T]] = {
		import c.universe._
		val tWtt: WeakTypeTag[T] = c.weakTypeTag[T];
		val tType: Type = tWtt.tpe;
		val tSymbol: Symbol = tType.typeSymbol;
		if (tSymbol.isClass) {
			val className: String = show(tType);
			val classSymbol = tSymbol.asClass;
			val paramsList = classSymbol.primaryConstructor.typeSignatureIn(tType).paramLists;

			val fieldsInfo = for {
				params <- paramsList
				param <- params
			} yield {
				val paramTerm = param.asTerm;

				q"""
					val ${paramTerm.name} = Parser.apply[${paramTerm.typeSignature}];
	   				builder.addOne((${paramTerm.name.toString}, read.ProductParserHelper.FieldInfo(${paramTerm.name}, None)));
				"""
			}

			val argsTermName = TermName("args")
			var argIndex = 0;
			val ctorArguments = for (params <- paramsList) yield {
				for (param <- params) yield {
					val paramTerm = param.asTerm;
					val argTree = q"$argsTermName($argIndex).asInstanceOf[${paramTerm.typeSignature}]";
					argIndex += 1;
					argTree
				}
			}
			val helper =
				q"""
import read.ProductParserHelper.FieldInfo;

val builder = scala.collection.immutable.ListMap.newBuilder[String, FieldInfo[_]];
..${fieldsInfo}

new ProductParserHelper[$tType] {
	override val className: String = $className;

	override val fieldsInfo: scala.collection.immutable.ListMap[String, FieldInfo[_]] =
		builder.result();

	override def createProduct($argsTermName: Seq[Any]):$tType =
		new ${tType.typeSymbol}[..${tType.dealias.typeArgs}](...${ctorArguments});
}""";
			c.echo(c.enclosingPosition, s"productHelper=$helper")

			c.Expr[ProductParserHelper[T]](helper)

		} else {
			c.warning(c.enclosingPosition, s"$tSymbol is not a class and only classes are supported")
			c.Expr[ProductParserHelper[T]](q"")
		}
	}

}
