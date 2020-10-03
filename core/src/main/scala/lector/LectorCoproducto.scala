package lector

package webServer.util.traductorJson

import scala.reflect.runtime.{universe => ru}

class LectorCoproducto[T](discriminador: String)(implicit typeTag: ru.TypeTag[T]) extends LectorJson {
	override def interpretar(puntero: Interpretador.Puntero): Nothing = ???
}
