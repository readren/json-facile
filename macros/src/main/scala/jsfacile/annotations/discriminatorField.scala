package jsfacile.annotations

import scala.reflect.macros.blackbox
import scala.reflect.{api => sra}

import jsfacile.joint.DiscriminatorConf

/** An annotation for sealed traits or abstract classes that:
 * (1) instructs the [[jsfacile.macros.CoproductAppenderMacro]] (which is the responsible to translate values declared with an abstract type to json representation) to append an extra field in the json representation of each instance of the annotated type, in order to specify the concrete type of the instance. The type is specified with the instance type's simple name.
 * (2) informs the [[jsfacile.macros.CoproductParserMacro]] (which is the responsible to translate values from json representation to instances of abstract data types) which is the name of the extra field that disambiguates between concrete candidates whose required fields have the same name.
 *
 * @param value the name of the extra field in the json representation, that specifies the type of the represented instance. */
class discriminatorField(value: String, required: Boolean = true) extends scala.annotation.StaticAnnotation


object discriminatorField {
	def parse[Ctx <: blackbox.Context](ctx: Ctx)(classSymbol: ctx.universe.ClassSymbol): Option[DiscriminatorConf] = {
		import ctx.universe._
		classSymbol.annotations.collectFirst {
			case a if a.tree.tpe =:= typeOf[jsfacile.annotations.discriminatorField] =>
				a.tree.children.tail match {
					case Literal(Constant(discriminatorFieldName: String)) :: argumentsTail =>
						val required = argumentsTail match {
							case Literal(Constant(r: Boolean)) :: _ => r // matches when the user specified the second annotation parameter without its name, independently if the annotation's second parameter is optional or not.
							case NamedArg(Ident(TermName("required")), Literal(Constant(r: Boolean))) :: _ => r // matches when the second annotation parameter is optional and the user specified it with its name
							case _ => ctx.abort(ctx.enclosingPosition, "The `required` argument of the `discriminatorField` annotation must be specified literally. An expression is not supported.")
						}
						DiscriminatorConf(discriminatorFieldName, required)

					case _ => ctx.abort(ctx.enclosingPosition, "The `value` argument of the `discriminatorField` annotation must be specified literally. An expression is not supported.")
				}
		}
	}
}