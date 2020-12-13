package jsfacile

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.{blackbox, whitebox}

import jsfacile.read.{Cursor, Parser}
import jsfacile.util.BitSet.BitSlot
import jsfacile.write.{Appender, Record}

package object macros {

	abstract class Lazy[Op[_]] {
		protected var instance: Op[Any] = _;
		def isEmpty: Boolean = instance == null;
		def set[P](op: Op[P]): Unit = this.instance = op.asInstanceOf[Op[Any]];
		def get[P]: Op[P] = this.asInstanceOf[Op[P]];

	}

	final class LazyParser extends Lazy[Parser] with Parser[Any] {
		override def parse(cursor: Cursor): Any = this.instance.parse(cursor)
	}

	final class LazyAppender extends Lazy[Appender] with Appender[Any] {
		def append(record: Record, a: Any): Record = this.instance.append(record, a);
	}


	/** Wraps a [[blackbox.Context.Type]] in order to be usable as a map key.
	 *
	 * Note: instances of this class exists only during compilation time.
	 *
	 * @param tpe a dealiased type */
	final class TypeKey(val tpe: blackbox.Context#Type) {
		override val toString: String = tpe.toString

		override def equals(other: Any): Boolean = other match {
			case that: TypeKey =>
				this.toString == that.toString &&
				tpe =:= that.tpe
			case _ => false
		}
		override val hashCode: Int = this.toString.hashCode
	}

	type TypeIndex = Int;

	/** Note: instances of this class exists only during compilation time. */
	trait Keeper {
		def setFailed(message: String): Unit;
		def addDependency(handler: Handler): Unit;
	}

	type HandlersMap = mutable.Map[TypeKey, Handler];
	/** This val is intended to be used by macros during compilation only */
	val appenderHandlersMap: HandlersMap = mutable.HashMap.empty;
	/** This val is intended to be used by macros during compilation only */
	val parserHandlersMap: HandlersMap = mutable.HashMap.empty;

	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the parser handlers that are capturing dependencies. */
	@inline def registerParserDependency(to: Handler): Unit = Handler.registerDependency(to, parserHandlersMap);
	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the appender handlers that are capturing dependencies. */
	@inline def registerAppenderDependency(to: Handler): Unit = Handler.registerDependency(to, appenderHandlersMap);

	@inline def showParserDependencies(handler: Handler): String = s"\nbuffered dependencies:${Handler.showDependenciesOf(handler, parserHandlersMap)}";
	@inline def showAppenderDependencies(handler: Handler): String = s"\nbuffered dependencies:${Handler.showDependenciesOf(handler, appenderHandlersMap)}";


	/**Note: instances of this class exists only during compilation time.
	 *
	 * @param fieldType the type of the field
	 * @param firstOwnerName the TypeSymbol of the first product that contains a field named as the key specifies.
	 * @param consideredFieldIndex the index of the field in the `consideredFields` parameter of the [[jsfacile.read.CoproductParser]] constructor.
	 * @param bitSlot the [[BitSlot]] assigned to this considered field*/
	case class ConsideredField(fieldType: scala.reflect.api.Types#Type, firstOwnerName: String, consideredFieldIndex: Int, bitSlot: BitSlot)


	//////////////////

	/** Checks if there is no macro call under the top of the macro call stack that satisfies the received predicate on the called macro full name.
	 *
	 * @return true if no macro call in the macro stack (excluding the top one) satisfies the predicate */
	private def isOuterMacroInvocation(ctx: blackbox.Context)(predicate: String => Boolean): Boolean = {
		import ctx.universe._;

		@tailrec
		def loop(head: blackbox.Context, tail: List[blackbox.Context]): Boolean = {
			var next = tail;
			// ignore immediate repetitions
			while (next.nonEmpty && next.head == head) next = next.tail;
			if (next.isEmpty) true
			else {
				val q"$term[..$_](...$_)" = head.macroApplication;
				val termSymbol = term.symbol;
				if (termSymbol.isMacro && predicate(termSymbol.fullName)) false
				else loop(next.head, next.tail)
			}
		}
		loop(ctx.enclosingMacros.head, ctx.enclosingMacros.tail)
	}
	def isOuterParserMacroInvocation(ctx: blackbox.Context): Boolean = {
		this.isOuterMacroInvocation(ctx) { methodName =>
			methodName == "jsfacile.read.PriorityLowParsers.jpCustom" ||
			methodName == "jsfacile.read.CoproductParserBuilder.add"  ||
			methodName == "jsfacile.read.CoproductParserBuilder.seal"
		}
	}
	def isOuterAppenderMacroInvocation(ctx: blackbox.Context): Boolean = {
		this.isOuterMacroInvocation(ctx) { _ == "jsfacile.write.PriorityLowAppenders.jaCustom" }
	}

	////////

	def showEnclosingMacros(ctx: blackbox.Context): String = {
		ctx.enclosingMacros.map { ctx =>
			if (false) s"\n\tapplication: ${ctx.macroApplication}, hashCode: ${ctx.hashCode}, name: "
			else {
				import ctx.universe._
				val q"$term[..$_](...$_)" = ctx.macroApplication
				s"""{
				   |		methodName : $term,
				   |		application: ${ctx.macroApplication},
				   |		actualType : ${ctx.prefix.actualType},
				   | 		prefix     : ${ctx.prefix},
				   |  		hashCode   : ${ctx.hashCode}
				   |	}""".stripMargin
			}
		}.mkString("\nmacros stack trace: [", ", ", "]")
	}

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		showEnclosingMacros(ctx)
		val implicits = ctx.enclosingImplicits.map { ic =>
			s"""|
				|	provider prefix: ${ic.pre},
				|	provider symbol: ${ic.sym.fullName},
				|	invoked type   : ${ic.pt},
				|	invoker code   : ${ic.tree}
				|""".stripMargin
		}.mkString("\nimplicits stack trace: [{", "},{", "}]")
		s"${showEnclosingMacros(ctx)}\n$implicits\n"
	}
}
