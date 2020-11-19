package jsfacile

import scala.collection.mutable
import scala.reflect.macros.whitebox


package object macros {

	type CoproductUpperBound = Any;
	type ProductUpperBound = Any;

	trait Lazy {
		def isEmpty: Boolean;
	};

	/** Wraps a [[whitebox.Context.Type]] in order to be usable as a map key.
	 *
	 * @param tpe a dealiased type */
	class TypeKey(val tpe: whitebox.Context#Type) {
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

	val appendersBufferSemaphore = new Object;
	val parsersBufferSemaphore = new Object;

	//////////////////

	def isOuterParserMacroInvocation(ctx: whitebox.Context): Boolean = {
		1 >= ctx.openImplicits.count { ic =>
			ic.sym.isMacro && (
				ic.sym.fullName == "jsfacile.macros.ProductParserHelper.materialize"
				|| ic.sym.fullName == "jsfacile.macros.CoproductParserHelper.materialize"
				)
		}
	}
	def isOuterAppenderMacroInvocation(ctx: whitebox.Context): Boolean = {
		1 >= ctx.openImplicits.count { ic =>
			ic.sym.isMacro && (
				ic.sym.fullName == "jsfacile.write.jaProduct"
				|| ic.sym.fullName == "jsfacile.macros.CoproductAppenderHelper.materialize"
				)
		}
	}

	def showOpenImplicitsAndMacros(ctx: whitebox.Context): String = {
		val macros = ctx.openMacros.map { ctx =>
			if (true) s"\n\thashCode: ${ctx.hashCode}, macroApp: ${ctx.macroApplication}"
			else
				s"""
				   |Context(
				   |	macroApp:	${ctx.universe.showRaw(ctx.macroApplication)} -> ${ctx.macroApplication},
				   |	actualType:	${ctx.universe.showRaw(ctx.prefix.actualType)} -> ${ctx.prefix.actualType},
				   | 	prefix:		${ctx.universe.showRaw(ctx.prefix)} -> ${ctx.prefix},
				   |  	hashCode:	${ctx.hashCode}
				   |)""".stripMargin
		}.mkString
		val implicits = ctx.openImplicits.map { ic =>
			if (true) s"${ic.sym.fullName} <- ${ic.tree}"
			else
				s"""|
				|ImplicitCandidate(
					|	pre: ${ic.pre},
					|	sym: ${ic.sym}, // name:${ic.sym.fullName}
					|	pt: ${ic.pt},
					|	tree: ${ic.tree}
					|)""".stripMargin
		}.mkString
		s"openMacros:$macros\nopenImplicits:$implicits\n"
	}
}
