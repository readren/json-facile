package jsfacile.macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

import jsfacile.read.{CoproductParser, Parser, SingletonParser}
import jsfacile.util.BitSet
import jsfacile.util.BitSet.BitSlot


object CustomParserMacro {

	def materializeImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Parser[T]] = {
		import ctx.universe._

		val customType: Type = ctx.weakTypeTag[T].tpe.dealias;
		val customSymbol: Symbol = customType.typeSymbol;

		if (!customSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$customSymbol is not a class.")
		}

		val customClassSymbol = customSymbol.asClass;
		if (customClassSymbol.isAbstract) {
			val coproductParserMacro = new CoproductParserMacro[ctx.type](ctx);
			coproductParserMacro.materializeImpl[T](customType, customClassSymbol);

		} else if (customClassSymbol.isModuleClass) {
			val body = q"""new _root_.jsfacile.read.SingletonParser[$customType](${customClassSymbol.module})"""
			ctx.Expr[SingletonParser[T]](body);

		} else {
			val productParserMacro = new ProductParserMacro[ctx.type](ctx);
			productParserMacro.materializeImpl[T](customType, customClassSymbol)
		}
	}

	//////////////////////////


	def buildParserBody[Ctx <: blackbox.Context](ctx: Ctx)(rootType: ctx.Type, rootHandler: Handler): ctx.Tree = {
		import ctx.universe._

		if (rootHandler.creationTreeOrErrorMsg.isDefined && isOuterParserMacroInvocation(ctx)) {
			val inits =
				for {
					(innerTypeKey, innerHandler) <- parserHandlersMap
					if rootHandler.doesDependOn(innerHandler.typeIndex)
				} yield {
					innerHandler.creationTreeOrErrorMsg.get match {
						case Right(parserCreationTree) =>
							q"""
val parserCreator = ${parserCreationTree.asInstanceOf[ctx.Tree]};
parsersBuffer(${innerHandler.typeIndex}).set(parserCreator.apply(parsersBuffer));"""

						case Left(innerErrorMsg) =>
							ctx.abort(ctx.enclosingPosition, s"""Unable to derive a parser for `$rootType` because it depends on the parser for `${innerTypeKey.toString}` whose derivation has failed saying: $innerErrorMsg""")
					}
				}

			q"""
import _root_.jsfacile.macros.LazyParser;

val parsersBuffer = _root_.scala.Array.fill(${parserHandlersMap.size})(new LazyParser);
{..$inits}
parsersBuffer(${rootHandler.typeIndex}).get[$rootType]"""

		} else {
			q"""parsersBuffer(${rootHandler.typeIndex}).get[$rootType]"""
		}
	}


	//////////////////////////

	/** Note: Instances of this class exists only during compilation time. */
	class BuildKeeper(typeIndex: TypeIndex) extends Handler(typeIndex) {
		val productAdditionTrees: mutable.ArrayBuffer[scala.reflect.api.Trees#Tree] = mutable.ArrayBuffer.empty;
		var nextBitSlot: BitSlot = BitSet.FIRST_BIT_SLOT;
		val metaConsideredFields: mutable.Map[String, ConsideredField] = mutable.Map.empty;
//		val accumOfAddCaseTrees: mutable.ArrayBuffer[scala.reflect.api.Trees#Tree] = mutable.ArrayBuffer.empty;
	}

	def addCaseImpl[C: ctx.WeakTypeTag, P: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Unit] = {
		import ctx.universe._

		val coproductType = ctx.weakTypeOf[C];
		val coproductSymbol = coproductType.typeSymbol;
		if (!coproductSymbol.isAbstract) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not an abstract class or trait")
		}
		val productType = ctx.weakTypeOf[P];
		val productSymbol = productType.typeSymbol;
		if (!productSymbol.isClass) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not a class.")
		}

		val keeper = getKeeper[ctx.type](ctx)(coproductType);

		keeper.creationTreeOrErrorMsg match {
			case None =>
				val coproductParserMacro = new CoproductParserMacro[ctx.type](ctx);
				keeper.nextBitSlot = coproductParserMacro.addProduct(productSymbol.asClass, coproductSymbol.asClass, coproductType, coproductType, keeper, keeper.nextBitSlot, keeper.metaConsideredFields, keeper.productAdditionTrees.asInstanceOf[mutable.ArrayBuffer[Tree]]);

//				val addCaseTree =
//					q"""
//val addCase: (Array[LazyParser], CoproductParserBuilderState[$coproductType]) => Unit = (parsersBuffer, state) => {
//	..${productAdditionTrees}
//};
//addCase""";

//				ctx.info(ctx.enclosingPosition, s"Coproduct parser builder for $coproductType case $productType adder: $addCaseTree", force = false);
//				keeper.accumOfAddCaseTrees.addOne(addCaseTree);
				ctx.Expr[Unit](q"()");

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"A parser for $coproductType was already built and sealed.")
		}
	}


	def sealImpl[C: ctx.WeakTypeTag](ctx: blackbox.Context)(discriminatorFieldName: ctx.Expr[String]): ctx.Expr[Parser[C]] = {
		import ctx.universe._

		val coproductType = ctx.weakTypeOf[C];
		val coproductSymbol = coproductType.typeSymbol;
		if (!coproductSymbol.isAbstract) {
			ctx.abort(ctx.enclosingPosition, s"$coproductSymbol is not an abstract class or trait")
		}

		val keeper = getKeeper[ctx.type](ctx)(coproductType);

		keeper.creationTreeOrErrorMsg match {
			case None =>

//				val appliedCasesAdditionsTrees = for {caseAdditionTree <- keeper.productAdditionTrees} yield
//						q"""
//val caseAdder = ${caseAdditionTree.asInstanceOf[ctx.Tree]};
//caseAdder.apply(parsersBuffer, state);"""

				val parserCreationTree =
					q"""
import _root_.scala.Array;
import _root_.jsfacile.joint.DiscriminatorDecider;
import _root_.jsfacile.macros.LazyParser;
import _root_.jsfacile.read.{Parser, CoproductParser, CoproductParserBuilderState};
import _root_.jsfacile.util.BitSet;
import CoproductParser.{CpProductInfo, CpFieldInfo, CpConsideredField};

val createParser: Array[LazyParser] => CoproductParser[$coproductType] = parsersBuffer => {
	val state = new CoproductParserBuilderState[$coproductType];

 	..${keeper.productAdditionTrees.asInstanceOf[mutable.Buffer[Tree]]}

	new CoproductParser[$coproductType](
		${coproductType.toString},
		$discriminatorFieldName,
		state.productsInfo,
		state.consideredFields,
		${keeper.nextBitSlot.shardIndex + 1}
	);
};
createParser"""

				keeper.creationTreeOrErrorMsg = Some(Right(parserCreationTree));
				// The result of the next type-check is discarded. It is called only to trigger the invocation of the macro calls contained in the given [[Tree]] which may add new [[Handler]] instances to the [[parserHandlersMap]], and this macro execution needs to know of them later.
				ctx.typecheck(parserCreationTree/*.duplicate*/) // the duplicate is necessary because, according to Dymitro Mitin, the `typeCheck` method mutates its argument sometimes.

			case Some(Left(errorCause)) =>
				ctx.abort(ctx.enclosingPosition, errorCause);

			case _ =>
				ctx.abort(ctx.enclosingPosition, s"A parser for $coproductType was already built and sealed.")
		}

		val body = buildParserBody(ctx)(coproductType, keeper);

		ctx.info(ctx.enclosingPosition, s"Coproduct parser builder for $coproductType seal: $body", force = false)
		ctx.Expr[CoproductParser[C]](body);
	}

	private def getKeeper[Ctx <: blackbox.Context](ctx: Ctx)(coproductType: ctx.Type): BuildKeeper = {
		val coproductTypeKey = new TypeKey(coproductType);
		parserHandlersMap.get(coproductTypeKey) match {
			case Some(keeper: BuildKeeper) =>
				keeper;

			case oHandler =>
				if (oHandler.isDefined) {
					ctx.info(ctx.enclosingPosition, s"The parser for `$coproductType` that was automatically derived previously will be replaced by the one resulting from this builder.", true)
				}
				val keeper = new BuildKeeper(parserHandlersMap.size);
				parserHandlersMap.put(coproductTypeKey, keeper)
				keeper

		}
	}
}
