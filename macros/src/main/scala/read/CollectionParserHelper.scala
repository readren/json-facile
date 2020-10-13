package read

import scala.collection.IterableFactory
import scala.collection.mutable.Builder
import scala.reflect.macros.blackbox


@deprecated
trait CollectionParserHelper[CC[E] <: CollectionParserHelper.LowerBound[E]] {
	def collectionBuilderFactory: IterableFactory[CC]
}


@deprecated
object CollectionParserHelper {

	type LowerBound[E] = Iterable[E];


//	/** Macro implicit materializer of [[ProductParserHelper]] instances. Ver [[https://docs.scala-lang.org/overviews/macros/implicits.html]] */
//	implicit def materialize[CC[E] <: LowerBound[E], E]: CollectionParserHelper[CC] = macro materializeImpl[CC, E]
//
//
//	def materializeImpl[CC[E] <: LowerBound[E], E](ctx: blackbox.Context)(implicit wtt: ctx.WeakTypeTag[CC]): ctx.Expr[CollectionParserHelper[CC[E]]] = {
//		import ctx.universe._
//		val ccWtt: WeakTypeTag[CC] = ctx.weakTypeTag[CC];
//		val cType: Type = ccWtt.tpe;
//		val cSymbol: Symbol = cType.typeSymbol;
//		if( cSymbol.isClass && cSymbol.companion.isModule) {
//			val moduleSymbol = cSymbol.companion.asModule;
//			val cTypeConstructor = cType.typeConstructor
////			if(moduleSymbol.typeSignature <:< ctx.typeOf[IterableFactory[]
//		}
//		???
//	}
}

