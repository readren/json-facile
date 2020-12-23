package jsfacile

import scala.collection.mutable
import scala.reflect.macros.{blackbox, whitebox}

import jsfacile.read.{Cursor, Parser}
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

	type HandlersMap = mutable.Map[TypeKey, Handler];
	/** This val is intended to be used by macros during compilation only */
	val appenderHandlersMap: HandlersMap = mutable.HashMap.empty;
	/** This val is intended to be used by macros during compilation only */
	val parserHandlersMap: HandlersMap = mutable.HashMap.empty;

	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the parser handlers that are capturing dependencies. */
	@inline def registerParserDependency(to: Handler): Unit = Handler.registerDependency(to, parserHandlersMap);
	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the appender handlers that are capturing dependencies. */
	@inline def registerAppenderDependency(to: Handler): Unit = Handler.registerDependency(to, appenderHandlersMap);

	def showParserDependencies(dependantHandler: Handler): String = s"\nbuffered dependencies:${Handler.show(parserHandlersMap, handler => dependantHandler.dependencies.contains(handler.typeIndex))}";
	def showAppenderDependencies(dependantHandler: Handler): String = s"\nbuffered dependencies:${Handler.show(appenderHandlersMap, handler => dependantHandler.dependencies.contains(handler.typeIndex))}";


	//////////////////

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		val implicits = ctx.enclosingImplicits.map { ic =>
			s"""|
				|	provider prefix: ${ic.pre},
				|	provider symbol: ${ic.sym.fullName},
				|	invoked type   : ${ic.pt},
				|	invoker code   : ${ic.tree}
				|""".stripMargin
		}.mkString("\nimplicits stack trace: [{", "},{", "}]")
		s"${new GenCommon(ctx).showEnclosingMacros}\n$implicits\n"
	}
}
