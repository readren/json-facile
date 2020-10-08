package lector

import scala.reflect.runtime.{universe => ru}

class LectorCoproducto[T <: AnyRef](discriminador: String)(implicit typeTag: ru.TypeTag[T]) extends Interpretador[T] {
	override def interpretar(puntero: Interpretador.Puntero): Nothing = ???
}
