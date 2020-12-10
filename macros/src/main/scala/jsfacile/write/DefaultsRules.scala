package jsfacile.write

import scala.reflect.runtime.{universe => ru}

import jsfacile.joint.MapUpperBound

/** Should be implemented by  */
trait DefaultsRules {

	/** The default [[MapFormatDecider]]. It is defined in this project because it uses the scala reflect API. */
	implicit def defaultMapFormatDecider[K](implicit ktt: ru.TypeTag[K]): MapFormatDecider[K, Any, MapUpperBound] =
		new MapFormatDecider[K, Any, MapUpperBound] {
			override val useObject: Boolean =
				ktt.tpe <:< ru.typeOf[CharSequence] || ktt.tpe =:= ru.definitions.IntTpe || ktt.tpe =:= ru.definitions.LongTpe || ktt.tpe =:= ru.definitions.CharTpe;
		}

}
