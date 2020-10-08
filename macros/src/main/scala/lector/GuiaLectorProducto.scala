package lector

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


trait GuiaLectorProducto[T <: AnyRef] {
	val className: String;
	val infoCampos: ListMap[String, GuiaLectorProducto.InfoCampo[_]];
	def crear(args: Seq[Any]): T
}

object GuiaLectorProducto {

	case class InfoCampo[C](interpretador: Interpretador[C], oValorPorOmision: Option[C], debug: Any) // TODO delete the debug field

	/** Invocador de instancias de Guia */
//	implicit def apply[T <: AnyRef](implicit guia: GuiaLectorProducto[T]): GuiaLectorProducto[T] = guia;

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
	def materializeGuiaImpl[T <: AnyRef : c.WeakTypeTag](c: blackbox.Context): c.Expr[GuiaLectorProducto[T]] = {
		import c.universe._
		val tWtt: WeakTypeTag[T] = c.weakTypeTag[T];
		val tType: Type = tWtt.tpe;
		val tSymbol: Symbol = tType.typeSymbol;
		if (tSymbol.isClass) {
			val className: String = show(tType)
			c.echo(c.enclosingPosition, s"Expandiendo GuiaLectorProducto[$className]")

			val ctorSymbol = tType.decl(termNames.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod).find(_.isPrimaryConstructor).get
			val paramLists = ctorSymbol.typeSignatureIn(tType).paramLists;
			c.echo(c.enclosingPosition, s"CtorSymbos=$ctorSymbol, paramsList=$paramLists")

			val invocacionesLectoresArgs = for {
				params <- paramLists
				param <- params
			} yield {
				val paramTerm = param.asTerm;
				val reified =
					if (param.typeSignatureIn(tType) <:< typeOf[Product])
						q"implicitly[lector.GuiaLectorProducto[${paramTerm.typeSignature}]]"
					else
						q"Interpretador.apply[${paramTerm.typeSignature}]"

				q"""
	 				import lector.LectorProductoTest._;
					val ${paramTerm.name} = Interpretador.apply[${paramTerm.typeSignature}];
	 				val borrame = scala.reflect.runtime.universe.reify($reified)
	   				builder.addOne((${paramTerm.name.toString}, lector.GuiaLectorProducto.InfoCampo(${paramTerm.name}, None, borrame)));
				"""
			}
			c.echo(c.enclosingPosition, s"invocacionesLectores=$invocacionesLectoresArgs")

			val argsTermName = TermName("args")
			var indiceArg = 0;
			val ctorArguments = for (params <- paramLists) yield {
				for (param <- params) yield {
					val paramTerm = param.asTerm;
					val argTree = q"$argsTermName($indiceArg).asInstanceOf[${paramTerm.typeSignature}]";
					indiceArg += 1;
					argTree
				}
			}
			c.echo(c.enclosingPosition, s"ctorArguments=$ctorArguments")

			val guia =
				q"""new GuiaLectorProducto[$tType] {
						override val className: String = $className;

	  					override val infoCampos: scala.collection.immutable.ListMap[String, lector.GuiaLectorProducto.InfoCampo[_]] = {
							val builder = scala.collection.immutable.ListMap.newBuilder[String, lector.GuiaLectorProducto.InfoCampo[_]];
							..${invocacionesLectoresArgs}
							builder.result();
						}

   						override def crear($argsTermName: Seq[Any]):$tType = {
		 					new ${tType.typeSymbol}[..${tType.dealias.typeArgs}](...${ctorArguments});
		 				}
	   				}
					"""
			c.echo(c.enclosingPosition, s"guia=$guia")

			c.Expr[GuiaLectorProducto[T]](guia)

		} else {
			c.Expr[GuiaLectorProducto[T]](q"")
//			c.error(c.enclosingPosition, s"$tSymbol is not a class and only classes are supported")
		}
	}

}
