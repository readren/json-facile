package jsfacile

import scala.collection.mutable

import jsfacile.joint.Coproduct
import jsfacile.macros.{CoproductParserHelper, ProductParserHelper, SingletonParserHelper}
import jsfacile.read.Parser.ignored

/** The implicits defined in this package object should NOT be imported in order to have less precedence than the implicit defined in the [[api.read]] package object, which should be imported.
 * The compiler finds the implicits defined here when it searches for instances of the [[Parser]] trait because it belongs to this package object. */
package object read {

	private val jpProductsCache = mutable.WeakHashMap.empty[String, ProductParser[_ <: Product]]

	implicit def jpProduct[P <: Product](implicit helper: ProductParserHelper[P]): ProductParser[P] = {
		jpProductsCache.getOrElseUpdate(
			helper.fullName,
			new ProductParser[P](helper)
		).asInstanceOf[ProductParser[P]]
	}

	private val jpCoproductsCache = mutable.WeakHashMap.empty[String, CoproductParser[_ <: Coproduct]]

	implicit def jpCoproduct[C <: Coproduct](implicit helper: CoproductParserHelper[C]): CoproductParser[C] = {
		jpCoproductsCache.getOrElseUpdate(
			helper.fullName,
			new CoproductParser[C](helper)
		).asInstanceOf[CoproductParser[C]]
	}


	implicit def jpSingleton[S](implicit helper: SingletonParserHelper[S]): Parser[S] = { cursor =>
		val ok = cursor.consumeChar('{') && cursor.consumeWhitespaces() && cursor.consumeChar('}');
		if(!ok) {
			cursor.fail(s"A json empty object was expected while parsing the singleton object ${helper.instance.getClass.getName}")
		}
		helper.instance
	}
}
