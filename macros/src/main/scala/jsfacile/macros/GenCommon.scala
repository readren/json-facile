package jsfacile.macros

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.api.builder.{ProductAppendingInfo, ProductParsingInfo}
import jsfacile.macros.GenCommon.{CoproductTranslatorsBuilderState, ProductAppendingInfoDigested_handmade, ProductAppendingInfoDigested_fromBuilder, ProductCustomization, ProductParsingInfoDigested, TypeKey, coproductsBuildersStates}

/**Contains classes whose instances are shared between macro executions.
 * Some of said instances contain information that depends on the context of the macro that created them. It is responsibility of said macros to limit themselves on which of said instances are they able to access to comply with the encapsulation established by the scala language access scope.
 *
 * @define compileTimeOnly Note: Instances of this class exists only during compilation time. */
object GenCommon {
	import scala.reflect.{api => sra};

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
				this.tpe =:= that.tpe
			case _ => false
		}
		override val hashCode: Int = this.toString.hashCode
	}


	class FieldAppendingInfo(val name: String, val tpe: sra.Types#Type, val accessor: sra.Trees#Tree)

	/** Knows the mutable state of a [[jsfacile.api.builder.CoproductTranslatorsBuilder.ProductAppendingInfoBuilder]] instance.
	 *
	 * $compileTimeOnly */
	class ProductAppendingInfoBuilderState {
		val fields: mutable.ArrayBuffer[FieldAppendingInfo] = mutable.ArrayBuffer.empty;
		var discriminatorValue: Option[String] = None;
	}

	/** Knows the information that the [[ParserBuilderMacro.sealParser]] macro method needs about a field of one of the products added with customized [[jsfacile.read.Parser]] derivation.
	 *
	 * $compileTimeOnly */
	class FieldParsingInfo(val name: String, val tpe: sra.Types#Type, val oDefaultValue: Option[sra.Trees#Tree])

	/** Knows the mutable state of a [[jsfacile.api.builder.CoproductTranslatorsBuilder.ProductParsingInfoBuilder]] instance.
	 *
	 * $compileTimeOnly */
	class ProductParsingInfoBuilderState[P] {
		val fields: mutable.ArrayBuffer[FieldParsingInfo] = mutable.ArrayBuffer.empty;
		var discriminatorValue: Option[String] = None;
		var ctorExpr: Option[sra.Exprs#Expr[Seq[Any] => P]] = None;
	}


	/**Digested version of [[ProductParsingInfo]].
	 * Knows the information that a [[ParserBuilderMacro.sealParser]] macro method needs about one of the products added with customized [[jsfacile.read.Parser]] derivation.
	 *
	 * $compileTimeOnly */
	class ProductParsingInfoDigested(val discriminatorValue: String, val tpe: sra.Types#Type, val fieldsInfo: Iterable[FieldParsingInfo], val ctor: sra.Trees#Tree);

	/**Digested version of the argument assigned to the `appendingInfo` parameter of the `jsfacile.builder.CoproductTranslatorsBuilder.add*]] method.
	 * Knows the information that a [[AppenderBuilderMacro.sealAppender]] macro method needs about one of the products added with customized [[jsfacile.write.Appender]] derivation.
	 *
	 * $compileTimeOnly */
	sealed trait ProductAppendingInfoDigested
	/**Digested version of the argument assigned to the `appendingInfo` parameter of the `jsfacile.builder.CoproductTranslatorsBuilder.add*]] method.
	 * Knows the information that a [[AppenderBuilderMacro.sealAppender]] macro method needs about one of the products added with customized [[jsfacile.write.Appender]] derivation.
	 *
	 * $compileTimeOnly */
	class ProductAppendingInfoDigested_handmade(val requiredFieldNames: Set[String], val appenderTree: sra.Trees#Tree) extends ProductAppendingInfoDigested;

	/**Digested version of the result of a call to [[jsfacile.builder.CoproductTranslatorsBuilder.ProductParsingInfoBuilder.complete]].
	 * Knows the information that a [[AppenderBuilderMacro.sealAppender]] macro method needs about one of the products added with customized [[jsfacile.write.Appender]] derivation.
	 *
	 * $compileTimeOnly */
	class ProductAppendingInfoDigested_fromBuilder(val discriminatorValue: String, val fields: Iterable[FieldAppendingInfo]) extends ProductAppendingInfoDigested;

	/** Knows the information that both, the [[ParserBuilderMacro.sealParser]] and the [[AppenderBuilderMacro.sealAppender]] macro methods, need about one of the added products.
	 *
	 * $compileTimeOnly */
	class ProductCustomization(val oAppendingInfo: Option[ProductAppendingInfoDigested], val oParsingInfo: Option[ProductParsingInfoDigested]);

	/** Intended to know the mutable state of a single instance of [[jsfacile.api.builder.CoproductTranslatorsBuilder]]. But actually it knows the merged state of all the instances of [[jsfacile.api.builder.CoproductTranslatorsBuilder]] of the same type. So, only one instance of [[jsfacile.api.builder.CoproductTranslatorsBuilder]][T] should exist simultaneously for each type `T`.
	 *
	 * $compileTimeOnly */
	class CoproductTranslatorsBuilderState {
		val appendingInfoBuilderStatePerProduct: mutable.Map[sra.Types#Type, ProductAppendingInfoBuilderState] = mutable.Map.empty
		val parsingInfoBuilderStatePerProduct: mutable.Map[sra.Types#Type, ProductParsingInfoBuilderState[_]] = mutable.Map.empty;
		val productsCollector: mutable.Map[sra.Types#Type, ProductCustomization] = mutable.Map.empty;
	}

	/** Knows all the [[CoproductTranslatorsBuilderState]] instances.
	 * CAUTION: Note that the [[CoproductTranslatorsBuilderState]] instances are not indexed by the identity of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] instances as it should be, but by the type of the [[jsfacile.api.builder.CoproductTranslatorsBuilder]] instead. Therefore, the state of all the instances of the same type is inconveniently merged. To avoid that, only one instance of [[jsfacile.api.builder.CoproductTranslatorsBuilder]][T] should exist for each type `T`. This limitation is not annoying because usually one instance is needed.
	 *
	 * This variable is used during compilation time only. */
	val coproductsBuildersStates: mutable.Map[TypeKey, CoproductTranslatorsBuilderState] = mutable.Map.empty;
}

/** Contains the macro methods that are common to both [[jsfacile.read.Parser]] and [[jsfacile.write.Appender]] derivation.
 *
 * Note: Instances of this class exists only during compilation time. */
class GenCommon[Ctx <: blackbox.Context](val ctx: Ctx) {
	import ctx.universe._;

	protected def getClassSymbol(tpe: Type): ClassSymbol = {
		val symbol = tpe.typeSymbol;
		if (!symbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$symbol is not a class.")
		}
		symbol.asClass
	}

	def addCase[P](coproductType: Type, productType: Type, oAppendingInfo: Option[Expr[ProductAppendingInfo[P]]], oParsingInfo: Option[Expr[ProductParsingInfo[P]]]): ctx.Expr[Unit] = {

		getClassSymbol(coproductType);
		getClassSymbol(productType);

		val coproductTypeKey = new TypeKey(coproductType);
		val coproductBuilderState = getCoproductTranslatorsBuilderStateOf(coproductTypeKey);

		val oAppendingInfoDigested =
			for (appendingInfo <- oAppendingInfo) yield {
				coproductBuilderState.appendingInfoBuilderStatePerProduct.get(productType) match {
					case Some(productAppendingInfoBuilderState) =>
						productAppendingInfoBuilderState.discriminatorValue match {
							case Some(discriminatorValue) =>
								new ProductAppendingInfoDigested_fromBuilder(discriminatorValue, productAppendingInfoBuilderState.fields)

							case None =>
								ctx.abort(ctx.enclosingPosition, "The received `appendingInfo` was not completed. The `ProductAppendingInfoBuilder.complete` method should have been called earlier.");
						}

					case None =>
						appendingInfo.tree match {
							case q"""$_.ProductAppendingInfo.apply[$_]($appenderTree)(..${requiredFieldsTrees})""" =>
								val requiredFields = for {
									fieldTree <- requiredFieldsTrees.asInstanceOf[List[Tree]]
								} yield fieldTree match {
									case Literal(Constant(fieldName: String)) => fieldName
									case _ => ctx.abort(ctx.enclosingPosition, s"The `requiredFieldNames` parameter of `ProductAppendingInfo` must be specified literally (literal strings separated by comas). Expression are not supported");
								}
								new ProductAppendingInfoDigested_handmade(requiredFields.toSet, appenderTree)

							case _ => ctx.abort(ctx.enclosingPosition, s"""The argument assigned to the `appendingInfo` parameter must be either: an instance of `ProductAppendingInfo[P]` created by the `ProductAppendingInfoBuilder.complete` method, or a literal expression with this form: `ProductAppendingInfo[$productType](<appender_expression>)("<field_1_Name>","<field_2_name>",..,"<field_N_name>")`.""");
						}
				}
			};

		val oParsingInfoDigested: Option[ProductParsingInfoDigested] =
			for (parsingInfo <- oParsingInfo) yield {
				coproductBuilderState.parsingInfoBuilderStatePerProduct.get(productType) match {
					case Some(productParsingInfoBuilderState) =>
						productParsingInfoBuilderState.discriminatorValue match {
							case Some(discriminatorValue) =>
								new ProductParsingInfoDigested(discriminatorValue, productType, productParsingInfoBuilderState.fields, productParsingInfoBuilderState.ctorExpr.get.tree.asInstanceOf[Tree])

							case None =>
								ctx.abort(ctx.enclosingPosition, "The received `parsingInfo` was not completed. The `ProductParsingInfoBuilder.complete` method should have benn called earlier.");
						}

					case None =>
						ctx.abort(ctx.enclosingPosition, "The `ProductParsingInfo` received in the `parsingInfo` argument was not created by a `ProductParsingInfoBuilder.complete` method, and it must.");
				}
			}

		if (coproductBuilderState.productsCollector.put(productType, new ProductCustomization(oAppendingInfoDigested, oParsingInfoDigested)).isDefined) {
			ctx.warning(ctx.enclosingPosition, s"The type `$productType` was already added to this coproduct builder. The previous information is replaced by this one.")
		}

		ctx.Expr[Unit](q"()");
	}

	def clearCases(coproductType: Type): ctx.Expr[Unit] = {
		getClassSymbol(coproductType);

		val coproductTypeKey = new TypeKey(coproductType);
		coproductsBuildersStates.put(coproductTypeKey, new CoproductTranslatorsBuilderState)
		ctx.Expr[Unit](q"()");
	}

	////////////////////

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

	protected def getCoproductTranslatorsBuilderStateOf(coproductTypeKey: TypeKey): CoproductTranslatorsBuilderState = {
		coproductsBuildersStates.getOrElseUpdate(coproductTypeKey, new CoproductTranslatorsBuilderState)
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
