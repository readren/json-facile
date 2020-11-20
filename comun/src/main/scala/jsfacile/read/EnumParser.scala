package jsfacile.read

import scala.collection.immutable.ArraySeq

import jsfacile.read.BasicParsers.{jpInt, jpString}
import jsfacile.read.Parser.ignored
import jsfacile.util.BinarySearch

class EnumParser[E <: scala.Enumeration](val enum: E) extends Parser[E#Value] {

	val values: ArraySeq[enum.Value] = ArraySeq.from(enum.values);

	/** The implementation should never call another [[Parser]] instance passing the cursor in failed or missed state. And therefore, can asume that the received cursor is {{{cursor.ok == true}}}. */
	override def parse(cursor: Cursor): E#Value = {
		if (cursor.have) {
			if (cursor.pointedElem == '"') {
				val name = jpString.parse(cursor);
				if (cursor.ok) {
					val index = values.indexWhere(_.toString == name)
					if (index >= 0) {
						values(index);
					} else {
						cursor.miss(s"""The expected enum "${enum.getClass.getName}" does not contain a value with this name: $name.""")
						ignored[E#Value]
					}
				} else {
					ignored[E#Value]
				}
			} else {
				val id = jpInt.parse(cursor)
				if (cursor.ok) {
					val value = BinarySearch.find(values.unsafeArray.asInstanceOf[Array[enum.Value]])(_.id - id)
					if (value == null) {
						cursor.miss(s"""The expected enum "${enum.getClass.getName}" does not contain a value with this id: $id.""")
					}
					value
				} else {
					cursor.miss(s"""A string with the name or an integer with the id of an element of the enum "${enum.getClass.getName}" was expected.""")
					ignored[E#Value]
				}
			}

		} else {
			cursor.miss(s"""A value of the enum "${enum.getClass.getName}" was expected but the end of the content was reached.""")
			ignored[E#Value]
		}
	}
}
