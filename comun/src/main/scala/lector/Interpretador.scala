package lector

import scala.collection.mutable

object Interpretador {

	type Elem = Int
	type Pos = Int

	val ELEM_IGNORADO: Elem = 0x8000_0000 // Elegí este pero pudo haber sido cualquier Int negativo. Debe ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.

	/** Type class que da una instancia cualquiera del tipo T. Usado para dar como resultado cuando la interpretación fracasa. */
	trait Ignora[@specialized T] {
		def ignorado: T
	}
	def ignorado[T](implicit ignora: Ignora[T]): T = ignora.ignorado;

	implicit val codePointIgnorado: Ignora[Elem] = new Ignora[Elem] {
		override def ignorado: Elem = ELEM_IGNORADO // Elegí este pero pudo haber sido cualquier Int negativo. Debe ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.
	}
	implicit def refIgnorada[T <: AnyRef]: Ignora[T] = IgnoraRef.asInstanceOf[Ignora[T]]
	object IgnoraRef extends Ignora[AnyRef] {
		override def ignorado: AnyRef = null.asInstanceOf[AnyRef]
	}



	trait Puntero {
		def pos: Pos;

		def ok: Boolean;
		def hay: Boolean
		def enFin: Boolean;
		def fracasado: Boolean;

		def elemApuntado: Elem
		def viene(esperado: String): Boolean

		def avanzar(cantPasos: Int = 1): Unit
		def fracasar(): Unit

		/** pone la marca de fallado */
		def fallar(): Unit
		/** ¿está puesta la marca de fallado? */
		def fallado: Boolean;
		/** quita las marcas de fracasado y fallado */
		def reparar(): Unit

		/** La implementación debe ejecutar el bloque recibido y, si luego este puntero queda fracasado sin falla, tiene también que recuperar la posición que tenía antes de ejecutar el bloque.
		 *
		 * @param bloque procedimiento que puede y suele alterar a este puntero.
		 * @return el resultado dado por el bloque, independientemente de como haya sido afectado este puntero. */
		def intentar[@specialized X](bloque: () => X): X
	}

	final case class ~[@specialized +A, @specialized +B](_1: A, _2: B) {
		override def toString = s"(${_1}~${_2})"
	}

	/** Siempre da el valor recibido, y quita la marca de fracaso si no hay marca de falla.
	 * Para obtener un éxito incondicional usar:
	 * {{{daExito.relajado}}} */
	def daExito[A](a: A): Interpretador[A] = { puntero =>
		if (!puntero.fallado) puntero.reparar();
		a
	}
	def daFracaso[A](implicit na: Ignora[A]): Interpretador[A] = {
		puntero =>
			puntero.fracasar();
			na.ignorado
	}

	def daPos: Interpretador[Pos] = {_.pos}

	implicit def aceptaElem(elem: Elem): Interpretador[Elem] = { (puntero: Puntero) =>
		if (puntero.hay && puntero.elemApuntado == elem) {
			puntero.avanzar()
			elem
		} else {
			puntero.fracasar()
			ELEM_IGNORADO
		}
	}
	implicit def aceptaChar(char: Char): Interpretador[Elem] = aceptaElem(char.toInt)

	implicit def aceptaStr(seq: String): Interpretador[String] = { (puntero: Puntero) =>
		if (puntero.viene(seq)) {
			puntero.avanzar(seq.length)
			seq
		} else {
			puntero.fracasar();
			ignorado[String]
		}
	}

	def aceptaElemSi(condicion: Elem => Boolean): Interpretador[Elem] = { (puntero: Puntero) =>
		if (puntero.hay) {
			val ea = puntero.elemApuntado;
			if (condicion(ea)) {
				puntero.avanzar()
				ea
			} else {
				puntero.fracasar();
				ELEM_IGNORADO
			}
		} else {
			puntero.fracasar()
			ELEM_IGNORADO
		}
	}

	def filtraYMapea[A](pf: PartialFunction[Elem, A])(implicit na: Ignora[A]): Interpretador[A] = { (puntero: Puntero) =>
		if (puntero.hay) {
			pf.applyOrElse(puntero.elemApuntado, (_: Elem) => {puntero.fracasar(); na.ignorado;})
		} else {
			na.ignorado
		}
	}

	/** Invocador de instancias de [[Interpretador]] */
	def apply[T](implicit i: Interpretador[T]): Interpretador[T] = i;
}

/** Tanto los métodos de este trait como los [[Interpretador]]es dados son thread safe. */
trait Interpretador[@specialized(Int) A] { self =>
	import Interpretador._

	def interpretar(puntero: Puntero): A

	def map[B](f: A => B)(implicit nb: Ignora[B]): Interpretador[B] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero);
		if (puntero.ok)
			f(a)
		else
			nb.ignorado
	}
	def ^^[B](f: A => B)(implicit nb: Ignora[B]): Interpretador[B] = map(f);
	def ^^^[B](b: B)(implicit nb: Ignora[B]): Interpretador[B] = map(_ => b)

	def flatMap[B](f: A => Interpretador[B])(implicit nb: Ignora[B]): Interpretador[B] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero)
		if (puntero.ok)
			f(a).interpretar(puntero)
		else
			nb.ignorado
	}
	def >>[B](f: A => Interpretador[B])(implicit nb: Ignora[B]): Interpretador[B] = flatMap(f);

	def seguido[B, C](iB: Interpretador[B])(f: (A, B) => C)(implicit na: Ignora[A], nb: Ignora[B], nc: Ignora[C]): Interpretador[C] = { puntero =>
		puntero.intentar { () =>
			val a = this.interpretar(puntero)
			if (puntero.ok) {
				val b = iB.interpretar(puntero)
				if (puntero.ok) {
					f(a, b)
				} else {
					nc.ignorado
				}
			} else {
				nc.ignorado
			}
		}
	}
	@inline def ~[B](iB: Interpretador[B])(implicit na: Ignora[A], nb: Ignora[B]): Interpretador[~[A, B]] = seguido(iB)(new ~(_, _))
	@inline def ~>[B](ib: Interpretador[B])(implicit na: Ignora[A], nb: Ignora[B]): Interpretador[B] = seguido(ib)((_, b) => b)
	@inline def <~[B](ib: Interpretador[B])(implicit na: Ignora[A], nb: Ignora[B]): Interpretador[A] = seguido(ib)((a, _) => a)


	def orElse[B >: A](iB: Interpretador[B])(implicit nb: Ignora[B]): Interpretador[B] = { (puntero: Puntero) =>
		val a = puntero.intentar { () => self.interpretar(puntero) };
		if (puntero.ok || puntero.fallado) {
			a
		} else {
			puntero.reparar();
			iB.interpretar(puntero)
		}
	}
	@inline def |[B >: A](iB: Interpretador[B])(implicit nb: Ignora[B]): Interpretador[B] = orElse(iB)

	def opt: Interpretador[Option[A]] = self.map(Some(_)) | daExito(None)

	private def repGenFunc[C](builder: mutable.Builder[A, C])(puntero: Puntero)(implicit nc: Ignora[C]): C = {
		var a = self.interpretar(puntero);
		while (puntero.ok) {
			builder.addOne(a);
			a = self.interpretar(puntero)
		}
		if (puntero.fallado) {
			nc.ignorado
		} else {
			puntero.reparar();
			builder.result()
		}
	}
	def repGen[C](builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignora[C]): Interpretador[C] = { (puntero: Puntero) =>
		this.repGenFunc(builderCtor())(puntero)
	}
	@inline def rep: Interpretador[List[A]] = repGen(() => List.newBuilder)

	def rep1Gen[C](builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignora[C]): Interpretador[C] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero);
		if (puntero.ok) {
			val builder = builderCtor();
			builder.addOne(a);
			repGenFunc(builder)(puntero)
		} else {
			nc.ignorado
		}
	}
	def rep1: Interpretador[List[A]] = rep1Gen(() => List.newBuilder)

	def repSepGen[B, C](iB: Interpretador[B], builder: mutable.Builder[A, C])(implicit na: Ignora[A], nb: Ignora[B], nc: Ignora[C]): Interpretador[C] = {
		val iB_self = iB ~> self;
		val iC: Interpretador[C] = { puntero =>
			val a = self.interpretar(puntero);
			if (puntero.ok) {
				builder.addOne(a);
				iB_self.repGenFunc(builder)(puntero)
			} else if (puntero.fallado) {
				nc.ignorado;
			} else {
				puntero.reparar();
				builder.result();
			}
		}
		iC
	}

	def repSep[B](iB: Interpretador[B])(implicit na: Ignora[A], nb: Ignora[B]): Interpretador[List[A]] = repSepGen(iB, List.newBuilder)

	def rep1SepGen[B, C](iB: Interpretador[B], builderCtor: () => mutable.Builder[A, C])(implicit na: Ignora[A], nb: Ignora[B], nc: Ignora[C]): Interpretador[C] = {
		val iB_self = iB ~> self
		val iC: Interpretador[C] = { puntero =>
			val a = self.interpretar(puntero);
			if (puntero.ok) {
				val builder = builderCtor();
				builder.addOne(a);
				iB_self.repGenFunc(builder)(puntero);
			} else {
				nc.ignorado
			}
		}
		iC
	}
	def rep1Sep[B](iB: Interpretador[B])(implicit na: Ignora[A], nb: Ignora[B]): Interpretador[List[A]] = rep1SepGen(iB, () => List.newBuilder)

	def repNGen[C](n: Int, builderCtor: () => mutable.Builder[A, C])(implicit nc: Ignora[C]): Interpretador[C] = { puntero =>
		if (n > 0) {
			puntero.intentar { () =>
				var a = self.interpretar(puntero);
				if (puntero.ok) {
					val builder = builderCtor();
					var cont = n;
					do {
						builder.addOne(a);
						cont -= 1;
						if (cont > 0) {
							a = self.interpretar(puntero)
						}
					} while (cont > 0 && puntero.ok)
					if (puntero.ok) {
						builder.result()
					} else {
						nc.ignorado
					}
				} else {
					nc.ignorado
				}
			}
		} else if (puntero.fallado) {
			nc.ignorado
		} else {
			builderCtor().result();
		}
	}
	def repN(n: Int): Interpretador[List[A]] = repNGen(n, () => List.newBuilder)

	/** Lanza falla si este [[Interpretador]] fracasa. */
	def orFail(implicit na: Ignora[A]): Interpretador[A] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero);
		if (puntero.ok) {
			a
		} else {
			puntero.fallar()
			na.ignorado;
		}
	}

	def recover[B >: A](b: B): Interpretador[B] = { puntero =>
		val a = self.interpretar(puntero);
		if (puntero.fallado ) {
			puntero.reparar()
			b
		} else {
			a
		}
	}

	/** Da un interpretador que, si no hay falla se comporta igual que este, y si hay falla la borra y se comporta como el interpretador recibido. */
	def recoverWith[B >: A](iB: Interpretador[B])(implicit nb: Ignora[B]): Interpretador[B] = { puntero =>
		val a = self.interpretar(puntero);
		if(puntero.fallado) {
			puntero.reparar();
			val b = iB.interpretar(puntero)
			if (puntero.ok) {
				b
			} else {
				nb.ignorado
			}
		} else {
			a
		}
	}
}



