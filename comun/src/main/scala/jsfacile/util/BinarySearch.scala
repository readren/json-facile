package jsfacile.util

object BinarySearch {

	def find[T <: AnyRef](array: Array[T])(criteria: T => Int): T = {
		var low: Int = 0;
		var high: Int = array.length;

		while (low <= high) {
			val mid: Int = (low + high) >>> 1;
			val midVal: T = array(mid);
			val cmp: Int = criteria(midVal);
			if (cmp < 0)
				low = mid + 1;
			else if (cmp > 0)
				high = mid - 1;
			else
				return midVal; // key found
		}
		null.asInstanceOf[T]; // key not found.
	}
}
