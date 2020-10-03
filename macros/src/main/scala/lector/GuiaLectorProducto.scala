package lector

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


trait GuiaLectorProducto[+T <: AnyRef] {
	val className: String;
	val infoCampos: ListMap[String, GuiaLectorProducto.InfoCampo[_ <: AnyRef]];
	def crear(args: Seq[AnyRef]): T
}

object GuiaLectorProducto {

	case class InfoCampo[T <: AnyRef](interpretador: Interpretador[T], oValorPorOmision: Option[T])

	/** Invocador de instancias de Guia */
	implicit def apply[T <: AnyRef](implicit guia: GuiaLectorProducto[T]): GuiaLectorProducto[T] = guia;

	/** Macro implicit materializer de instancias de [[GuiaLectorProducto]] que extienden [[AnyRef]]. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
	implicit def materializeGuia[T <: AnyRef]: GuiaLectorProducto[T] = macro materializeGuiaImpl[T]

	/** Ejemplo del código que genera este macro si fuera invocado así: {{{
	 *	case class Person[W](name: String, age: Int, work: W);
	 *	materializeGuia[Person[Double]];
	 * }}}
	 * Resultado:
	 * {{{
	 *	new GuiaLectorProducto[Person[Double]] {
	 *		override val className: String = className;
	 *
	 *		override val infoCampos: ListMap[String, InfoCampo[_ <: Any]] = {
	 *
	 *			val builder = ListMap.newBuilder[String, InfoCampo[_ <: Any]];
	 *
	 *			val name = LectorJson.apply[String];
	 *			builder.addOne(("name", InfoCampo(name, None)));
	 *			val age = LectorJson.apply[Int];
	 *			builder.addOne(("age", InfoCampo(age, None)));
	 *			val work = LectorJson.apply[Double];
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
	def materializeGuiaImpl[T <: AnyRef](c: blackbox.Context)(implicit wtt: c.WeakTypeTag[T]): c.Expr[GuiaLectorProducto[T]] = {
		import c.universe._
		val tpe: Type = wtt.tpe
		val className: String = tpe.toString
		c.echo(c.enclosingPosition, s"Expandiendo GuiaLectorProducto[$className]")

		val ctorSymbol = tpe.decl(termNames.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod).find(_.isPrimaryConstructor).get
		val paramLists = ctorSymbol.typeSignatureIn(tpe).paramLists;
		c.echo(c.enclosingPosition, s"CtorSymbos=$ctorSymbol, paramsList=$paramLists")

		val invocacionesLectoresArgs = for {
			params <- paramLists
			param <- params
		} yield {
			val paramTerm = param.asTerm
			q"""
					val ${paramTerm.name} = LectorJson.apply[${paramTerm.typeSignatureIn(tpe)}];
	   				builder.addOne(${paramTerm.name.toString}, ${paramTerm.name})
				"""
		}
		c.echo(c.enclosingPosition, s"invocacionesLectores=$invocacionesLectoresArgs")

		val argsTermName = TermName("args")
		var indiceArg = 0;
		val ctorArguments = for (params <- paramLists) yield {
			for (param <- params) yield {
				val paramTerm = param.asTerm;
				val argTree = q"$argsTermName($indiceArg).asInstanceOf[${paramTerm.typeSignatureIn(tpe)}]";
				indiceArg += 1;
				argTree
			}
		}
		c.echo(c.enclosingPosition, s"ctorArguments=$ctorArguments")

		val guia =
			q"""new GuiaLectorProducto[$tpe] {
						override val className: String = $className;

	  					override val infoCampos: ListMap[String, InfoCampo[_ <: AnyRef]] = {
							val builder = ListMap.newBuilder[String, InfoCampo[_ <: AnyRef]];
							..${invocacionesLectoresArgs}
							builder.result();
						}

   						override def crear($argsTermName: Seq[AnyRef]):$tpe = {
		 					new ${tpe.typeSymbol}[${tpe.dealias.typeArgs}](...${ctorArguments});
		 				}
	   				}
					"""
		c.echo(c.enclosingPosition, s"guia=$guia")

		c.Expr[GuiaLectorProducto[T]](guia)
	}

}
