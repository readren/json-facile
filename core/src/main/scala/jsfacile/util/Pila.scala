package jsfacile.util

sealed abstract class Pila {
	def ::[@specialized T](tope: T): ::[T, this.type] = new ::(tope, this)
}

final case class ::[@specialized +T, +P <: Pila](tope: T, pila: P) extends Pila

case object Base extends Pila

//// ---- ////

sealed abstract class PilaAcotada[B] {
	//
	def #:[T <: B](tope: T): #:[B, T, this.type] = new #:(tope, this)
}

final case class #:[B, +T <: B, +P <: PilaAcotada[B]](tope: T, pila: P) extends PilaAcotada[B]

final case class Suelo[B]() extends PilaAcotada[B]

//// ---- ////

object Pruebas {
	def main(param: Array[String]): Unit = {

		val pila = "Hola" :: 3 :: true :: Base
		val x = pila.pila.pila.tope

		//		val y1: Suelo[Boolean] = Suelo[Boolean]()
		//		val y2 = "Hola" #: y1  /// Suelo[Boolean].#:("Hola") -> #:[Boolean, String, Suelo[Boolean]]#:("Hola", Suelo[Boolean])   ## T:String, B: Boolean
		//		val y3 = 3 #: y2
		//		println(y3);
	}

}
