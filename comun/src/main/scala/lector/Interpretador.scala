package lector

import scala.collection.mutable


object Interpretador {

	type Elem = Int
	type Pos = Int


	trait Nulable[@specialized(Int) T] {
		def nulo: T
		def esNulo(t: T): Boolean = t == nulo
	}
	def nulo[T](implicit nt: Nulable[T]): T = nt.nulo

	implicit val codePointNulo: Nulable[Elem] = new Nulable[Elem] {
		override def nulo: Elem = 0x8000_0000 // Elegí este pero pudo haber sido cualquier Int negativo. Debe ser mayor a 0x10FFFF para que sea un code point inválido, y menor a cero para que sea una posición inválida.
	}
	implicit def refNula[T <: AnyRef]: Nulable[T] = new Nulable[T] {
		override def nulo: T = null.asInstanceOf[T];
	}


	trait Puntero {
		def pos: Pos;

		def elemApuntado: Elem
		def viene(esperado: String): Boolean

		/** avanza y borra la marca de falla */
		def avanzar(cantPasos: Int = 1): Unit
		/** retrocede y borra la marca de falla */
		def retroceder(): Unit
		def ponerEnFalla(estado: Boolean): Unit

		def enFin: Boolean;
		def enFalla: Boolean;
		def ok: Boolean = !enFin && !enFalla

		def tomarFotoEstado: Long
		def revertir(estado: Long): this.type
	}

	final case class ~[@specialized +A, @specialized +B](_1: A, _2: B) {
		override def toString = s"(${_1}~${_2})"
	}

	def daExito[A](a: A): Interpretador[A] = { _ => a }
	def daFracaso[A](implicit na: Nulable[A]): Interpretador[A] = { _ => nulo[A] }

	def daPosicion: Interpretador[Pos] = {_.pos}

	implicit def aceptaElem(elem: Elem): Interpretador[Elem] = { (puntero: Puntero) =>
		if (puntero.ok && puntero.elemApuntado == elem) {
			puntero.avanzar()
			elem
		} else
			nulo[Elem]
	}
	implicit def aceptaChar(char: Char): Interpretador[Elem] = aceptaElem(char.toInt)

	implicit def aceptaStr(seq: String): Interpretador[String] = { (puntero: Puntero) =>
		if (puntero.viene(seq)) {
			puntero.avanzar(seq.length)
			seq
		} else
			nulo[String]
	}

	def aceptaElemSi(condicion: Elem => Boolean): Interpretador[Elem] = { (puntero: Puntero) =>
		if (puntero.ok) {
			val ea = puntero.elemApuntado;
			if (condicion(ea)) {
				puntero.avanzar()
				ea
			} else
				nulo[Elem]
		} else {
			nulo[Elem]
		}
	}

	def filtraYMapea[A](pf: PartialFunction[Elem, A])(implicit na: Nulable[A]): Interpretador[A] = { (puntero: Puntero) =>
		if (puntero.ok) {
			val a = pf.applyOrElse(puntero.elemApuntado, (_: Elem) => nulo[A])
			if (a != nulo[A]) {
				puntero.avanzar()
			}
			a
		} else {
			nulo[A]
		}
	}

	/** Invocador de instancias de [[Interpretador]] */
	def apply[T](implicit i: Interpretador[T]): Interpretador[T] = i;
}


trait Interpretador[@specialized(Int) A] { self =>
	import Interpretador._

	def interpretar(puntero: Puntero): A

	def map[B](f: A => B)(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero)
		if (a != nulo[A])
			f(a)
		else
			nulo[B]
	}
	def ^^[B](f: A => B)(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = map(f);

	def flatMap[B](f: A => Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero)
		if (a != nulo[A])
			f(a).interpretar(puntero)
		else
			nulo[B]
	}
	def >>[B](f: A => Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = flatMap(f);

	def seguido[B, C](iB: Interpretador[B])(f: (A, B) => C)(implicit na: Nulable[A], nb: Nulable[B], nc: Nulable[C]): Interpretador[C] = { (puntero: Puntero) =>
		val fotoEstado = puntero.tomarFotoEstado;
		val a = this.interpretar(puntero)
		if (a != nulo[A]) {
			val b = iB.interpretar(puntero)
			if (b != nulo[B])
				f(a, b)
			else {
				puntero.revertir(fotoEstado)
				nulo[C]
			}
		} else
			nulo[C]
	}
	def ~[B](iB: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[~[A, B]] = seguido(iB)((a, b) => new ~(a, b))
	def ~>[B](ib: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = seguido(ib)((_, b) => b)
	def <~[B](ib: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[A] = seguido(ib)((a, _) => a)


	def |[B >: A](iB: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[B] = { (puntero: Puntero) =>
		val fotoEstado = puntero.tomarFotoEstado;
		val a = self.interpretar(puntero)
		if (a != nulo[A])
			a
		else {
			iB.interpretar(puntero.revertir(fotoEstado))
		}
	}

	def opt(implicit na: Nulable[A]): Interpretador[Option[A]] = self.map(Some(_)) | daExito(None)

	def repGen[C](builder: mutable.Builder[A, C])(implicit na: Nulable[A]): Interpretador[C] = { (puntero: Puntero) =>
		val nuloA = nulo[A];
		var a = self.interpretar(puntero);
		while (a != nuloA) {
			builder.addOne(a);
			a = self.interpretar(puntero)
		}
		builder.result()
	}
	def rep(implicit na: Nulable[A]): Interpretador[List[A]] = repGen(List.newBuilder)

	def rep1Gen[C](builderCtor: () => mutable.Builder[A, C])(implicit na: Nulable[A], nc: Nulable[C]): Interpretador[C] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero);
		if (a == nulo[A]) {
			nulo[C]
		} else {
			val builder = builderCtor();
			builder.addOne(a);
			this.repGen[C](builder).interpretar(puntero)
		}
	}
	def rep1(implicit na: Nulable[A]): Interpretador[List[A]] = rep1Gen(() => List.newBuilder)

	def repSepGen[B, C](iB: Interpretador[B], builder: mutable.Builder[A, C])(implicit na: Nulable[A], nb: Nulable[B], nc: Nulable[C]): Interpretador[C] = { puntero =>
		val a = self.interpretar(puntero);
		if (a == nulo[A]) {
			builder.result();
		} else {
			builder.addOne(a)
			(iB ~> self).repGen(builder).interpretar(puntero)
		}
	}
	def repSep[B](iB: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[List[A]] = repSepGen(iB, List.newBuilder)

	def rep1SepGen[B, C](iB: Interpretador[B], builderCtor: () => mutable.Builder[A, C])(implicit na: Nulable[A], nb: Nulable[B], nc: Nulable[C]): Interpretador[C] = { puntero =>
		val a = self.interpretar(puntero);
		if (a == nulo[A]) {
			nulo[C]
		} else {
			val builder = builderCtor();
			builder.addOne(a);
			(iB ~> self).repGen(builder).interpretar(puntero);
		}
	}
	def rep1Sep[B](iB: Interpretador[B])(implicit na: Nulable[A], nb: Nulable[B]): Interpretador[List[A]] = rep1SepGen(iB, () => List.newBuilder)

	def repNGen[C](n: Int, builderCtor: () => mutable.Builder[A, C])(implicit na: Nulable[A], nc: Nulable[C]): Interpretador[C] = { puntero =>
		if (n <= 0) {
			builderCtor().result();
		} else {
			val nuloA = nulo[A];
			val fotoEstado = puntero.tomarFotoEstado;
			var a = self.interpretar(puntero);
			if (a == nuloA) {
				nulo[C]
			} else {
				val builder = builderCtor();
				var cont = n;
				do {
					builder.addOne(a);
					cont -= 1;
					if (cont > 0) {
						a = self.interpretar(puntero)
					}
				} while (cont > 0 && a != nuloA)
				if (a != nuloA) {
					builder.result()
				} else {
					puntero.revertir(fotoEstado);
					nulo[C]
				}
			}
		}
	}
	def repN(n: Int)(implicit na: Nulable[A]): Interpretador[List[A]] = repNGen(n, () => List.newBuilder)

	def estricto(implicit na: Nulable[A]): Interpretador[A] = { (puntero: Puntero) =>
		val a = self.interpretar(puntero);
		if (a == nulo[A]) {
			puntero.ponerEnFalla(true)
		}
		a
	}

	def relajado: Interpretador[A] = { puntero =>
		puntero.ponerEnFalla(false)
		self.interpretar(puntero)
	}

	def preveeExito: Interpretador[A] = { (puntero: Puntero) =>
		val fotoEstado = puntero.tomarFotoEstado;
		val a = self.interpretar(puntero);
		puntero.revertir(fotoEstado);
		a
	}

	def preveeFracaso(implicit na: Nulable[A]): Interpretador[java.lang.Boolean] = { (puntero: Puntero) =>
		val fotoEstado = puntero.tomarFotoEstado;
		val a = self.interpretar(puntero);
		if (a != nulo[A]) {
			puntero.revertir(fotoEstado);
			nulo[java.lang.Boolean]
		} else {
			java.lang.Boolean.TRUE;
		}
	}
}



