package read

import scala.reflect.runtime.{universe => ru}

class CoproductoReader[T <: AnyRef](discriminator: String)(implicit typeTag: ru.TypeTag[T]) extends Parser[T] {
	override def parse(puntero: Parser.Cursor): Nothing = ???
}
