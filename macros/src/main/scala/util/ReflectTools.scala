package util

import scala.reflect.{api => sra}

object ReflectTools {

	/** Given a type `baseType` and a type constructor of one of its direct subclasses `directSubclassTypeConstructor`, creates a type by applying said type constructor to the type arguments that were used to create the `baseType` as seen from said direct subclass.
	 * @param baseType a type resulting of the instantiation of a type constructor. For example: {{{typeOf[Option[Int]]}}}
	 * @param directSubclassTypeConstructor the type constructor we want to instantiate such that it is assignable to `baseType`. For example: {{{typeOf[Some[_]].typeConstructor}}}
	 * @return the type constructed by applying the type constructor `directSubclassTypeConstructor` to the type arguments of `baseType` as seen from said type constructor. For example: {{{typeOf[Some[Int]]}}}*/
	def applySubclassTypeConstructor(universe: sra.Types with sra.Symbols)(baseType: universe.Type, directSubclassTypeConstructor: universe.Type): universe.Type = {
		val directSubclassTypeParams = directSubclassTypeConstructor.typeParams
		if( directSubclassTypeParams.isEmpty) {
			directSubclassTypeConstructor
		} else {
			val baseTypeConstructor = baseType.typeConstructor;
			assert(directSubclassTypeConstructor <:< baseTypeConstructor)

			val subclassTypeParamsToBaseTypeArgumentsRelationship =
				for {
					(baseTypeParam, baseTypeArgument) <- baseTypeConstructor.typeParams zip baseType.typeArgs
				} yield {
					val directSubclassTypeParam = baseTypeParam.asType.toType.asSeenFrom(directSubclassTypeConstructor, baseType.typeSymbol)
					directSubclassTypeParam -> baseTypeArgument
				}

			val directSubclassTypeArguments =
				for (subclassTypeParm <- directSubclassTypeParams) yield {
					subclassTypeParamsToBaseTypeArgumentsRelationship.find { r =>
						r._1.typeSymbol.name == subclassTypeParm.name
					}.get._2
				}

			universe.appliedType(directSubclassTypeConstructor, directSubclassTypeArguments)
		}
	}

}
