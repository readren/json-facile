package jsfacile.macros

import scala.collection.mutable
import scala.reflect.api.{Universe => univ}

import jsfacile.macros.GenCommon.TypeKey
import jsfacile.macros.Handler.HandlersMap


/** Keeps the state of the generation of the code that builds an [[jsfacile.write.Appender]] or a [[jsfacile.read.Parser]] for the type indicated by the received [[TypeIndex]]; and manages the inter-dependence of the associated type with the types associated to other [[Handler]] instances.
 * Note: Instances of this class exists only during compilation time.*/
class Handler(val typeIndex: TypeIndex) {
	/** The code lines that creates a [[jsfacile.read.Parser]] or a [[jsfacile.write.Appender]].*/
	var creationTreeOrErrorMsg: Option[Either[String, univ#Tree]] = None;

	/** Is 'true' while this [[Handler]] is open to add more dependencies to its dependency set.
	 * Tells the [[Handler.registerDependency]] method that it have to add any [[TypeIndex]] it receives to the [[dependencies]] set of this [[Handler]].
	 * This flag is set to `false` by the macro when it knows that the handlers of all the inner types have been created and added to the corresponding [[HandlersMap]]. */
	var isCapturingDependencies: Boolean = true;

	/** the set indexes of the handlers on which this handled  depends, including himself. A handler depends on other handler when the type definition associated to the first contains a reference to the type definition associated to the second. */
	val dependencies: mutable.BitSet = mutable.BitSet(typeIndex) // includes himself

	private val dependants: mutable.Set[Handler] = mutable.Set.empty // does not include himself

	def setFailed(cause: String): Unit = this.creationTreeOrErrorMsg = Some(Left(cause));

	def setCreationTree(tree: univ#Tree): Unit = this.creationTreeOrErrorMsg = Some(Right(tree));

	def addDependency(dependency: Handler): Unit = {
		dependency.addDependant(this);
		if (!dependency.dependencies.subsetOf(this.dependencies)) {
			this.dependencies |= dependency.dependencies;
			dependants.foreach(_.addIndirectDependencies(dependency.dependencies));
		}
	}
	private def addDependant(dependant: Handler): Unit = {
		this.dependants.add(dependant);
	}
	private def addIndirectDependencies(newDependencies: mutable.BitSet): Unit = {
		if (!newDependencies.subsetOf(this.dependencies)) {
			this.dependencies |= newDependencies;
			dependants.foreach(_.addIndirectDependencies(newDependencies))
		}
	}
	@inline def doesDependOn(typeIndex: TypeIndex): Boolean = {
		this.dependencies.contains(typeIndex);
	}

	def clear(handlersMap: HandlersMap): Unit = {
		creationTreeOrErrorMsg = None;
		isCapturingDependencies = true;
		this.dependencies.clear();
		this.dependencies.add(typeIndex);
		this.dependants.clear();
		// delete this handler from the set of dependants of other handlers
		for ((_, aHandler) <- handlersMap) {
			aHandler.dependants.remove(this)
		}
	}


	override def equals(other: Any): Boolean = other match {
		case that: Handler =>
			typeIndex == that.typeIndex
		case _ => false
	}
	override def hashCode(): Int = typeIndex

	override def toString: String = s"Handler(index=${this.typeIndex}${creationTreeOrErrorMsg match {case Some(Left(msg)) => s", error=$msg" case _ => ""}})"
}

object Handler {

	type HandlersMap = mutable.Map[TypeKey, Handler];
	/** This val is intended to be used by macros during compilation only */
	val appenderHandlersMap: HandlersMap = mutable.HashMap.empty;
	/** This val is intended to be used by macros during compilation only */
	val parserHandlersMap: HandlersMap = mutable.HashMap.empty;

	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the parser handlers that are capturing dependencies. */
	@inline def registerParserDependency(to: Handler): Unit = Handler.registerDependency(to, parserHandlersMap);
	/** Adds the received [[TypeIndex]] to the [[Handler.dependencies]] set of all the appender handlers that are capturing dependencies. */
	@inline def registerAppenderDependency(to: Handler): Unit = Handler.registerDependency(to, appenderHandlersMap);

	def registerDependency(to: Handler, handlersMap: HandlersMap): Unit = {
		for {(_, handler) <- handlersMap} {
			if (handler.isCapturingDependencies) handler.addDependency(to);
		}
	}

	/////////

	def getCleanHandlerFor(typeKey: TypeKey, handlersMap: HandlersMap): Handler = {
		handlersMap.get(typeKey) match {
			case None =>
				val typeKeyIndex = handlersMap.size;
				val handler = new Handler(typeKeyIndex)
				handlersMap.put(typeKey, handler);
				handler

			case Some(handler) =>
				handler.clear(handlersMap);
				handler
		}
	}

	/////////

	def showParserDependencies(dependantHandler: Handler): String = s"\nbuffered dependencies:${Handler.show(parserHandlersMap, handler => dependantHandler.dependencies.contains(handler.typeIndex))}";
	def showAppenderDependencies(dependantHandler: Handler): String = s"\nbuffered dependencies:${Handler.show(appenderHandlersMap, handler => dependantHandler.dependencies.contains(handler.typeIndex))}";

	def show(handlersMap: HandlersMap, filter: Handler => Boolean): String = {
		val tc = for {
			(keyType, handler) <- handlersMap
			if filter(handler)
		} yield {
			handler.typeIndex -> f"name: ${keyType.toString.takeRight(50)}%50s, expanded: ${handler.creationTreeOrErrorMsg.exists(_.isRight)}%5.5b, capturing: ${handler.isCapturingDependencies}%5.5b, dependencies: ${handler.dependencies.mkString(", ")}%s"
		}
		mutable.SortedMap.from(tc);
		tc.mkString("\n\t", "\n\t", "")
	}
}
