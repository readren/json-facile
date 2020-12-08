package jsfacile.macros

import scala.collection.mutable
import scala.reflect.api.{Universe => univ}


/** Manages the construction and inter-relation of an [[jsfacile.write.Appender]] or [[jsfacile.read.Parser]] of the type indicated by the received [[TypeIndex]].
 * This class is intended to be used by macros during compilation only*/
class Handler(val typeIndex: TypeIndex) {
	/** The code lines that creates a [[jsfacile.read.Parser]] or a [[jsfacile.write.Appender]].*/
	var creationTreeOrErrorMsg: Option[Either[String, univ#Tree]] = None;

	/** Is 'true' while this [[Handler]] is open to add more dependencies to its dependency set.
	 * Tells the [[Handler.registerDependency]] method that it have to add any [[TypeIndex]] it receives to the [[dependencies]] set of this [[Handler]].
	 * This flag is set to `false` by the macro when it knows that the handlers of all the inner [[Type]]s have been created and added to the corresponding [[HandlersMap]]. */
	var isCapturingDependencies: Boolean = true;

	/** the set indexes of the handlers on which this handled  depends, including himself. A handler depends on other handler when the type definition associated to the first contains a reference to the type definition associated to the second. */
	private val dependencies: mutable.BitSet = mutable.BitSet(typeIndex) // include himself

	private val dependants: mutable.Set[Handler] = mutable.Set.empty

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

	override def equals(other: Any): Boolean = other match {
		case that: Handler =>
			typeIndex == that.typeIndex
		case _ => false
	}
	override def hashCode(): Int = typeIndex
}

object Handler {

	def registerDependency(to: Handler, handlersMap: HandlersMap): Unit = {
		for {(_, handler) <- handlersMap} {
			if (handler.isCapturingDependencies) handler.addDependency(to);
		}
	}

	/////////

	def showDependenciesOf(dependantHandler: Handler, handlersMap: HandlersMap): String = {
		val tc = for {
			(keyType, handler) <- handlersMap
			if dependantHandler.dependencies.contains(handler.typeIndex)
		} yield {
			handler.typeIndex -> f"name: ${keyType.toString.takeRight(50)}%50s, expanded: ${handler.creationTreeOrErrorMsg.exists(_.isRight)}%5.5b, capturing: ${handler.isCapturingDependencies}%5.5b, dependencies: ${handler.dependencies.mkString(", ")}%s"
		}
		mutable.SortedMap.from(tc);
		tc.mkString("\n\t", "\n\t", "")
	}
}
