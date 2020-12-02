package jsfacile.macros

import scala.reflect.{api => sra}

object ReflectTools {

	/** Given a type `baseType` and a type constructor of one of its direct subclasses `directSubclassTypeConstructor`, creates a type by applying said type constructor to the type arguments that were used to create the `baseType` as seen from said direct subclass.
	 *
	 * An in depth explanation of what this function does can be found in the stack overflow question [[https://stackoverflow.com/questions/64504396/find-out-the-type-arguments-for-a-type-constructor-knowing-the-type-arguments-of]] which I did and later responded to myself :)
	 *
	 * @param baseType                      a type resulting of the instantiation of a type constructor. For example: {{{typeOf[Option[Int]]}}}
	 * @param directSubclassTypeConstructor the type constructor we want to instantiate such that it is assignable to `baseType`. For example: {{{typeOf[Some[_]].typeConstructor}}}
	 * @return if all the type parameters of the `directSubclassTypeConstructor` depend on the base type constructor (has no free type parameters), the type constructed by applying the type constructor `directSubclassTypeConstructor` to the type arguments of `baseType` as seen from said type constructor; for example: {{{typeOf[Some[Int]]}}}. Otherwise, returns a list with the subclass free type parameters. */
	def applySubclassTypeConstructor[U <: sra.Universe](universe: U)(baseType: universe.Type, directSubclassTypeConstructor: universe.Type): Either[List[universe.Symbol], universe.Type] = {
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

			val directSubclassTypeArgumentsBuilder = List.newBuilder[universe.Type]
			var subclassFreeTypeParams: List[universe.Symbol] = Nil
			for (subclassTypeParam <- directSubclassTypeParams) {
				subclassTypeParamsToBaseTypeArgumentsRelationship.find { r =>
					r._1.typeSymbol.name == subclassTypeParam.name
				} match {
					case Some((_, baseTypeArgument)) => directSubclassTypeArgumentsBuilder.addOne(baseTypeArgument)
					case None => subclassFreeTypeParams = subclassTypeParam :: subclassFreeTypeParams
				}
			}
			if (subclassFreeTypeParams.isEmpty) {
				Right(universe.appliedType(directSubclassTypeConstructor, directSubclassTypeArgumentsBuilder.result()).ensuring(_ <:< baseType))
			} else {
				Left(subclassFreeTypeParams)
			}
		}
	}

}
