package jsfacile

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.whitebox

import jsfacile.joint.Named
import jsfacile.read.{Cursor, Parser}
import jsfacile.write.{Appender, Record}

package object macros {

	@inline def namedOrdering[T <: Named]: Ordering[T] = Ordering.by[T, String](_.name)

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


	/** Wraps a [[whitebox.Context.Type]] in order to be usable as a map key.
	 *
	 * @param tpe a dealiased type */
	final class TypeKey(val tpe: whitebox.Context#Type) {
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

	type HandlersMap = mutable.Map[TypeKey, Handler];
	val appenderHandlersMap: HandlersMap = mutable.HashMap.empty;
	val parserHandlersMap: HandlersMap = mutable.HashMap.empty;

	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the parser handlers that are capturing dependencies. */
	@inline def registerParserDependency(to: Handler): Unit = Handler.registerDependency(to, parserHandlersMap);
	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the appender handlers that are capturing dependencies. */
	@inline def registerAppenderDependency(to: Handler): Unit = Handler.registerDependency(to, appenderHandlersMap);

	@inline def showParserHandlers: String = Handler.show(parserHandlersMap);
	@inline def showAppenderHandlers: String = Handler.show(appenderHandlersMap);

	//////////////////

	/** Checks if there is no macro call under the top of the macro call stack that satisfies the received predicate on the called macro full name.
	 *
	 * @return true if no macro call in the macro stack (excluding the top one) satisfies the predicate */
	private def isOuterMacroInvocation(ctx: whitebox.Context)(predicate: String => Boolean): Boolean = {
		import ctx.universe._;

		@tailrec
		def loop(head: whitebox.Context, tail: List[whitebox.Context]): Boolean = {
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
	def isOuterParserMacroInvocation(ctx: whitebox.Context): Boolean = {
		this.isOuterMacroInvocation(ctx) { methodFullName =>
			methodFullName == "jsfacile.read.PriorityLowParsers.jpProduct" || methodFullName == "jsfacile.read.PriorityLowParsers.jpCoproduct"
		}
	}
	def isOuterAppenderMacroInvocation(ctx: whitebox.Context): Boolean = {
		this.isOuterMacroInvocation(ctx) { methodFullName =>
			methodFullName == "jsfacile.write.PriorityLowAppenders.jaProduct" || methodFullName == "jsfacile.write.PriorityLowAppenders.jaCoproduct"
		}
	}

	////////

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		val macros = ctx.openMacros.map { ctx =>
			if (true) s"\n\thashCode: ${ctx.hashCode}, macroApp: ${ctx.macroApplication}"
			else {
				import ctx.universe._
				val q"$term[..$_](...$_)" = ctx.macroApplication
				s"""
				   |Context(
				   |	macroApp:	${ctx.macroApplication} -> ${ctx.universe.showRaw(ctx.macroApplication)},
				   |	actualType:	${ctx.prefix.actualType} -> ${ctx.universe.showRaw(ctx.prefix.actualType)},
				   | 	prefix:		${ctx.prefix} -> ${ctx.universe.showRaw(ctx.prefix)},
				   |  	hashCode:	${ctx.hashCode}
				   |)""".stripMargin
			}
		}.mkString
		val implicits = ctx.openImplicits.map { ic =>
			if (false) s"\n\t${ic.sym.fullName} <- ${ic.tree}"
			else
				s"""|
				|ImplicitCandidate(
					|	pre: ${ic.pre},
					|	sym: ${ic.sym}, // name:${ic.sym.fullName}
					|	pt: ${ic.pt},
					|	tree: ${ic.tree}
					|)""".stripMargin
		}.mkString
		s"\nopenMacros:$macros\n\nopenImplicits:$implicits\n"
	}
}
