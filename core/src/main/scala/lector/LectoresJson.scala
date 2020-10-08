package lector

import scala.collection.immutable.ListMap
import scala.collection.mutable

/** Tanto los métodos de este objeto como los [[Interpretador]]es dados por ellos son thread safe. */
object LectoresJson {
	import Interpretador._

	private val MAX_LONG_DIV_10 = java.lang.Long.MAX_VALUE / 10;
	private val MAX_INT_DIV_10 = java.lang.Integer.MAX_VALUE / 10;
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

	val skipSpaces: Interpretador[Pos] = { puntero =>
		while (Character.isWhitespace(puntero.elemApuntado)) {
			puntero.avanzar()
		}
		puntero.pos
	}
	val colon: Interpretador[Elem] = ':'
	val coma: Interpretador[Elem] = ','

	private def normalChar: Interpretador[Elem] = aceptaElemSi(c => c != '\"' && c != '\\' && c >= 0x20 && c <= 0x10ffff)
	private def hexDigit: Interpretador[Elem] = aceptaElemSi(c => ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'))
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
	val string: Interpretador[String] = '\"' ~> (normalChar | escape).repGen { () => new CodePointStrBuilder } <~ '\"'

	private val digit: Interpretador[Elem] = aceptaElemSi(Character.isDigit)
	private val skipDigits: Interpretador[Pos] = { puntero =>
		while (Character.isDigit(puntero.elemApuntado)) {
			puntero.avanzar();
		}
		puntero.pos
	}
	private val digit19: Interpretador[Elem] = aceptaElemSi(e => '1' <= e && e <= '9')
	private val skipInteger: Interpretador[Pos] = '-'.opt ~> (aceptaChar('0') ~> daPos) | (digit19 ~> skipDigits)
	private val skipFraction: Interpretador[Pos] = '.' ~> digit ~> skipDigits
	private val skipExponent: Interpretador[Pos] = ('e' | 'E') ~> ('+' | '-').opt ~> digit ~> skipDigits // me parece que en lugar de "digit" debería ser "digit19", pero me regí por la página https://www.json.org/json-en.html

	private def skipJsNumber: Interpretador[Pos] = skipInteger ~> skipFraction.opt ~> skipExponent.opt ~> daPos
	private def skipJsNull: Interpretador[Pos] = "null" ~> daPos
	private def skipJsBoolean: Interpretador[Pos] = ("true" | "false") ~> daPos
	private def skipJsString: Interpretador[Pos] = string ~> daPos
	private def skipJsArray: Interpretador[Pos] = '[' ~> skipSpaces ~> (skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> ']' ~> daPos
	private def skipJsObject: Interpretador[Pos] = '{' ~> skipSpaces ~> (string ~> skipSpaces ~> colon ~> skipSpaces ~> skipJsValue ~> skipSpaces).repSep(coma ~> skipSpaces) ~> '}' ~> daPos
	val skipJsValue: Interpretador[Pos] = new Interpretador[Pos] {
		private var mem: Interpretador[Pos] = _
		override def interpretar(puntero: Puntero): Pos = {
			if (mem == null) {
				this.mem = skipJsString | (skipJsArray | (skipJsObject | (skipJsNull | (skipJsBoolean | skipJsNumber))))
			}
			this.mem.interpretar(puntero)
		}
	}

	trait LectoresJsonLowPriority {

//		implicit def materializeGuia[T <: AnyRef]: GuiaLectorProducto[T] = new GuiaLectorProducto[T] {
//			override val className: String = "Borrame"
//			override val infoCampos: ListMap[String, GuiaLectorProducto.InfoCampo[_]] = ListMap.empty
//			override def crear(args: Seq[Any]): T = ???
//		}

		/** Invocador de instancias de [[LectorProducto]] */
		implicit def ijClass[C <: AnyRef]: Interpretador[C] = new LectorProducto[C]

	}
}


/**  Interpretadores para tipos nativos de scala */
trait LectoresJson extends LectoresJson.LectoresJsonLowPriority {
	import LectoresJson._

	implicit val ijString: Interpretador[String] = LectoresJson.string;

	/** Interpretador de Int en Json */
	implicit val ijInt: Interpretador[Int] = { puntero =>
		if (puntero.ok) {
			puntero.intentar { () =>
				var acum: Int = 0;
				var limit = 9; // longitud de la mantisa
				var elemApuntado = puntero.elemApuntado;
				val esNegativo = elemApuntado == '-';
				if (esNegativo) {
					puntero.avanzar()
					elemApuntado = puntero.elemApuntado;
				};
				var digit = elemApuntado - '0';
				if (digit < 0 || 9 < digit) {
					puntero.fracasar();
				} else {
					do {
						acum = acum * 10 + digit;
						limit -= 1;
						puntero.avanzar();
						elemApuntado = puntero.elemApuntado;
						digit = elemApuntado - '0';
					} while (0 <= digit && digit <= 9 && limit > 0);

					if (0 <= digit && digit <= 9) {
						if (acum > MAX_INT_DIV_10 || (acum == MAX_INT_DIV_10 && (digit == 9 || !esNegativo && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
							puntero.fracasar();
						} else {
							acum = acum * 10 + digit
							puntero.avanzar();
							elemApuntado = puntero.elemApuntado;
							if ('0' <= elemApuntado && elemApuntado <= '9') {
								puntero.fracasar();
							}
						}
					}
				}
				if (esNegativo) -acum
				else acum
			}
		} else {
			0
		}
	}

	/** Interpretador de Long en Json */
	implicit val ijLong: Interpretador[Long] = { puntero =>
		if (puntero.ok) {
			puntero.intentar { () =>
				var acum: Long = 0;
				var limit = 18; // longitud de la mantisa

				var elemApuntado = puntero.elemApuntado;
				val esNegativo = elemApuntado == '-';
				if (esNegativo) {
					puntero.avanzar()
					elemApuntado = puntero.elemApuntado;
				};
				var digit = elemApuntado - '0';
				if (digit < 0 || 9 < digit) {
					puntero.fracasar();
				} else {
					do {
						acum = acum * 10L + digit;
						limit -= 1;
						puntero.avanzar();
						elemApuntado = puntero.elemApuntado;
						digit = elemApuntado - '0';
					} while (0 <= digit && digit <= 9 && limit > 0);

					if (0 <= digit && digit <= 9) {
						if (acum > MAX_LONG_DIV_10 || (acum == MAX_LONG_DIV_10 && (digit == 9 || !esNegativo && digit == 8))) { // notar que MAX_LONG/10 == MIN_LONG/10. De lo contrario habría que lidiar con la diferencia.
							puntero.fracasar();
						} else {
							acum = acum * 10 + digit
							puntero.avanzar();
							elemApuntado = puntero.elemApuntado;
							if ('0' <= elemApuntado && elemApuntado <= '9') {
								puntero.fracasar();
							}
						}
					}
				}
				if (esNegativo) -acum
				else acum
			}
		} else {
			0L
		}
	}
}






