package lector

import scala.collection.mutable

object LectorJson {
	import Interpretador._

	private val MIN_LONG_DIV_10 = java.lang.Long.MIN_VALUE / 10;
	private val MIN_INT_DIV_10 = java.lang.Integer.MIN_VALUE / 10;


	//	def apply[T](implicit inter: LectorJson[T]): LectorJson[T] = inter;

	class MissingFieldException(className: String, fieldName: String) extends RuntimeException(s"class: $className, field: $fieldName")

	class CodePointStrBuilder extends mutable.Builder[Int, String] {
		private val sb = new java.lang.StringBuilder()
		override def clear(): Unit = sb.setLength(0);
		override def result(): String = sb.toString;
		override def addOne(elem: Int): CodePointStrBuilder.this.type = {
			sb.appendCodePoint(elem);
			this;
		};
	}

	/** Invocador de instancias de [[LectorJson]] */
	def apply[T <: AnyRef](implicit i: LectorJson[T]): LectorJson[T] = i;

	implicit val ijInt: LectorJson[java.lang.Integer] = { puntero =>
		if (puntero.ok) {
			var acum: Int = 0;
			var ok = true;
			var limit = -java.lang.Integer.MAX_VALUE;

			var elemApuntado = puntero.elemApuntado;
			val esNegativo = elemApuntado == '-';
			if (esNegativo) {
				limit = java.lang.Integer.MIN_VALUE;
				puntero.avanzar()
				elemApuntado = puntero.elemApuntado;
			};
			var digit = Character.digit(elemApuntado, 10);
			if(esNegativo && (digit < 0 || 9 < digit)) {
				puntero.retroceder();
				ok = false;
			}
			while (0 <= digit && digit <= 9 && ok) {
				// si multiplicar por 10 no causará overflow
				if (acum >= MIN_INT_DIV_10) {
					acum = acum * 10;
					// si sumar el dígito no causará overflow
					if (acum >= limit + digit) {
						acum -= digit;
						puntero.avanzar();
						elemApuntado = puntero.elemApuntado;
						digit = elemApuntado - '0';
					} else {
						ok = false;
					}
				} else {
					ok = false;
				}
			}
			if (ok) {
				if (esNegativo) acum
				else -acum
			} else {
				//				// dado que Int no tiene nulo, poner el puntero en falla.
				//				puntero.ponerEnFalla(true);
				nulo[java.lang.Integer]
			}
		} else
			nulo[java.lang.Integer];
	}

	implicit val ijLong: LectorJson[java.lang.Long] = { puntero =>
		if (puntero.ok) {
			var acum: Long = 0;
			var ok = true;
			var limit = -java.lang.Long.MAX_VALUE;

			var elemApuntado = puntero.elemApuntado;
			val esNegativo = elemApuntado == '-';
			if (esNegativo) {
				limit = java.lang.Long.MIN_VALUE;
				puntero.avanzar()
				elemApuntado = puntero.elemApuntado;
			};
			var digit = elemApuntado - '0';
			if(esNegativo && (digit < 0 || 9 < digit)) {
				puntero.retroceder();
				ok = false;
			}
			while (0 <= digit && digit <= 9 && ok) {
				// si multiplicar por 10 no causará overflow
				if (acum >= MIN_LONG_DIV_10) {
					acum = acum * 10L;
					// si sumar el dígito no causará overflow
					if (acum >= limit + digit) {
						acum -= digit;
						puntero.avanzar();
						elemApuntado = puntero.elemApuntado;
						digit = elemApuntado - '0';
					} else {
						ok = false;
					}
				} else {
					ok = false;
				}
			}
			if (ok) {
				if (esNegativo) acum
				else -acum
			} else {
				//				// dado que Long no tiene nulo, poner el puntero en falla.
				//				puntero.ponerEnFalla(true);
				nulo[java.lang.Long]
			}
		} else
			nulo[java.lang.Long];
	}

}

import LectorJson._

abstract class LectorJson[T <: AnyRef] extends Interpretador[T] {
	import Interpretador._

	protected def skipSpaces: Interpretador[Pos] = { puntero =>
		while (Character.isWhitespace(puntero.elemApuntado)) {
			puntero.avanzar()
		}
		puntero.pos
	}
	protected def colon: Interpretador[Elem] = skipSpaces ~> ':'
	protected def coma: Interpretador[Elem] = skipSpaces ~> ','

	private def normalChar: Interpretador[Elem] = aceptaElemSi(c => c != '\"' && c != '\\' && c >= 0x20 && c <= 0x10ffff)
	private def hexDigit: Interpretador[Elem] = aceptaElemSi(c => Character.isDigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'))
	private def hexCode: Interpretador[Elem] = 'u' ~> hexDigit.repNGen(4, () => new CodePointStrBuilder) ^^ { hex => Integer.parseInt(hex, 16) }
	private def escape: Interpretador[Elem] = '\\' ~> filtraYMapea {
		case '"' => '"'.toInt
		case '\\' => '\\'.toInt
		case '/' => '/'.toInt
		case 'b' => '\b'.toInt
		case 'f' => '\f'.toInt
		case 'n' => '\n'.toInt
		case 'r' => '\r'.toInt
		case 't' => '\t'.toInt
	} | hexCode
	protected def string: Interpretador[String] = skipSpaces ~> '\"' ~> (normalChar | escape).repGen(new CodePointStrBuilder) <~ '\"'

	private def digit: Interpretador[Elem] = aceptaElemSi(Character.isDigit)
	private def skipDigits: Interpretador[Pos] = { puntero =>
		while (Character.isDigit(puntero.elemApuntado)) {
			puntero.avanzar();
		}
		puntero.pos
	}
	private def digit19: Interpretador[Elem] = aceptaElemSi(e => '1' <= e && e <= '9')
	private def skipInteger: Interpretador[Pos] = '-'.opt ~> (aceptaChar('0') ~> daPosicion) | (digit19 ~> skipDigits)
	private def skipFraction: Interpretador[Pos] = '.' ~> digit ~> skipDigits
	private def skipExponent: Interpretador[Pos] = ('e' | 'E') ~> ('+' | '-').opt ~> digit ~> skipDigits // me parece que en lugar de "digit" debería ser "digit19", pero me regí por la página https://www.json.org/json-en.html

	protected def skipJsNumber: Interpretador[Pos] = skipSpaces ~> skipInteger ~> skipFraction.opt ~> skipExponent.opt ~> daPosicion
	protected def skipJsNull: Interpretador[Pos] = skipSpaces ~> "null" ~> daPosicion
	protected def skipJsBoolean: Interpretador[Pos] = skipSpaces ~> ("true" | "false") ~> daPosicion
	protected def skipJsString: Interpretador[Pos] = string ~> daPosicion
	protected def skipJsArray: Interpretador[Pos] = skipSpaces ~> '[' ~> skipJsValue.repSep(coma) ~> ']' ~> daPosicion
	protected def skipJsObject: Interpretador[Pos] = skipSpaces ~> '{' ~> (string ~> colon ~> skipJsValue).repSep(coma) ~> skipSpaces ~> '}' ~> daPosicion
	protected def skipJsValue: Interpretador[Pos] = skipJsString | skipJsArray | skipJsObject | skipJsNull | skipJsBoolean | skipJsNumber

}







