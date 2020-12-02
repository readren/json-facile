package jsfacile.read

import jsfacile.joint.IterableUpperBound
import jsfacile.read.Parser._
import jsfacile.util.NonVariantHolderOfAnIterableFactory

/** @tparam IC iterator type constructor
 * @tparam E   element's type */
class IterableParser[IC[e] <: IterableUpperBound[e], E](
	parserE: Parser[E],
	factoryHolder: NonVariantHolderOfAnIterableFactory[IC] // Asking for the IterableFactory directly would fail because it is Covariant which causes the compiler to pick the most specialized instance. And here we want the compiler to pick the instance of the specified type. So we wrap IterableFactory with a non variant holder.
) extends Parser[IC[E]] {
	assert(parserE != null && factoryHolder != null);

	override def parse(cursor: Cursor): IC[E] = {
		if (cursor.have) {
			if (cursor.pointedElem == '[') {
				cursor.advance();
				val builder = factoryHolder.factory.newBuilder[E];
				var have = cursor.consumeWhitespaces();
				while (have && cursor.pointedElem != ']') {
					have = false;
					val value = parserE.parse(cursor);
					if (cursor.consumeWhitespaces()) {
						builder.addOne(value);
						have = cursor.pointedElem == ']' || (cursor.consumeChar(',') && cursor.consumeWhitespaces());
					}
				}
				if (have) {
					cursor.advance();
					builder.result()
				} else {
					cursor.fail(s"Invalid syntax for iterable. The builder factory is ${factoryHolder.factory.getClass.getName}");
					ignored[IC[E]]
				}
			} else {
				cursor.miss(s"An iterable opening char was expected.")
				ignored[IC[E]]
			}
		} else {
			cursor.miss("A iterable opening char was expected but the end of the content was reached.")
			ignored[IC[E]]

		}

	}
}
