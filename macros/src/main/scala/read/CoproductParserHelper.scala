package read


object CoproductParserHelper {
	trait Coproduct;

}

import CoproductParserHelper._

trait CoproductParserHelper[T <: Coproduct] {
	def discriminator: String;
}

