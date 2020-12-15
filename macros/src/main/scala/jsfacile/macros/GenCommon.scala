package jsfacile.macros

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.macros.GenCommon.{BuildKeeper, buildKeepersMap}


object GenCommon {
	import scala.reflect.{api => sra};

	/** Note: Instances of this class exists only during compilation time. */
	class BuildKeeper {
		val productsCollector: mutable.Set[sra.Types#Type] = mutable.Set.empty;
	}

	val buildKeepersMap: mutable.Map[TypeKey, BuildKeeper] = mutable.Map.empty;
}

class GenCommon[Ctx <: blackbox.Context](val ctx: Ctx) {
	import ctx.universe._;

	/** Checks if there is no macro call under the top of the macro call stack that satisfies the received predicate on the called macro full name.
	 *
	 * @return true if no macro call in the macro stack (excluding the top one) satisfies the predicate */
	protected def isOuterMacroInvocation(predicate: String => Boolean): Boolean = {

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

	////////////////////

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

	///////////////////

	def addCase[P](coproductType: Type, productType: Type): ctx.Expr[Unit] = {

		val coproductSymbol = coproductType.typeSymbol;
		if (!coproductSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a class.")
		}
		val productSymbol = productType.typeSymbol;
		if (!productSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a class.")
		}

		val coproductTypeKey = new TypeKey(coproductType);
		val keeper = getKeeper(coproductTypeKey);
		if(!keeper.productsCollector.add(productType)) {
			ctx.warning(ctx.enclosingPosition, s"T This operation has no effect. because the $productType is already included to the set of considered subtypes of $coproductType.")
		}
		ctx.Expr[Unit](q"()");
	}


	protected def getKeeper(coproductTypeKey: TypeKey): BuildKeeper = {
		buildKeepersMap.getOrElseUpdate(coproductTypeKey, new BuildKeeper)
	}

	/////////////////////


	/** Given a type `baseType` and a type constructor of one of its direct subclasses `directSubclassTypeConstructor`, creates a type by applying said type constructor to the type arguments that were used to create the `baseType` as seen from said direct subclass.
	 *
	 * An in depth explanation of what this function does can be found in the stack overflow question [[https://stackoverflow.com/questions/64504396/find-out-the-type-arguments-for-a-type-constructor-knowing-the-type-arguments-of]] which I did and later responded to myself :)
	 *
	 * @param baseType                      a type resulting of the instantiation of a type constructor. For example: {{{typeOf[Option[Int]]}}}
	 * @param directSubclassTypeConstructor the type constructor we want to instantiate such that it is assignable to `baseType`. For example: {{{typeOf[Some[_]].typeConstructor}}}
	 * @return if all the type parameters of the `directSubclassTypeConstructor` depend on the base type constructor (has no free type parameters), the type constructed by applying the type constructor `directSubclassTypeConstructor` to the type arguments of `baseType` as seen from said type constructor; for example: {{{typeOf[Some[Int]]}}}. Otherwise, returns a list with the subclass free type parameters. */
	def applySubclassTypeConstructor(baseType: Type, directSubclassTypeConstructor: Type): Either[List[Symbol], Type] = {
		val directSubclassTypeParams = directSubclassTypeConstructor.typeParams
		if (directSubclassTypeParams.isEmpty) {
			Right(directSubclassTypeConstructor)
		} else {
			val baseTypeConstructor = baseType.typeConstructor;

			val subclassTypeParamsToBaseTypeArgumentsRelationship =
				for {
					(baseTypeParam, baseTypeArgument) <- baseTypeConstructor.typeParams zip baseType.typeArgs
				} yield {
					// find out how is a type parameter `baseTypeParam` of the base type constructor `baseTypeConstructor` seen from the `directSubclassTypeConstructor` when said base type constructor is instantiated as `baseType`.
					val directSubclassTypeParam = baseTypeParam.asType.toType.asSeenFrom(directSubclassTypeConstructor, baseType.typeSymbol)
					directSubclassTypeParam -> baseTypeArgument
				}

			val directSubclassTypeArgumentsBuilder = List.newBuilder[Type]
			var subclassFreeTypeParams: List[Symbol] = Nil
			for (subclassTypeParam <- directSubclassTypeParams) {
				subclassTypeParamsToBaseTypeArgumentsRelationship.find { r =>
					r._1.typeSymbol.name == subclassTypeParam.name
				} match {
					case Some((_, baseTypeArgument)) => directSubclassTypeArgumentsBuilder.addOne(baseTypeArgument)
					case None => subclassFreeTypeParams = subclassTypeParam :: subclassFreeTypeParams
				}
			}
			if (subclassFreeTypeParams.isEmpty) {
				Right(appliedType(directSubclassTypeConstructor, directSubclassTypeArgumentsBuilder.result()).ensuring(_ <:< baseType))
			} else {
				Left(subclassFreeTypeParams)
			}
		}
	}


	/////////////////////

	def showEnclosingMacros: String = {
		ctx.enclosingMacros.map { ctx =>
			if (false) s"\n\tapplication: ${ctx.macroApplication}, hashCode: ${ctx.hashCode}, name: "
			else {
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
}
