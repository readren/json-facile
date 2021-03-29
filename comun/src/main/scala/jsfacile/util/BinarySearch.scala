package jsfacile.util

object BinarySearch {

	/** Finds the element of the array such that the `criteria` function return zero.
	 * @param sortedArray an array sorted with a criteria compatible with the received criteria.
	 * @param size number of elements of the array that are relevant. The remaining are not considered for the search.
	 * @param criteria a function that receives one of the elements contained in the array and should return zero if it is the searched one, a positive number if it has a higher index than the searched one, or a negative number otherwise. */
	def find[T <: AnyRef](sortedArray: Array[T], size: Int)(criteria: FuncToInt[T]): T = {
		var low: Int = 0;
		var high: Int = size;

		while (low < high) {
			val mid: Int = (low + high) >>> 1;
			val midVal: T = sortedArray(mid);
			val cmp: Int = criteria(midVal);
			if (cmp < 0)
				low = mid + 1;
			else if (cmp > 0)
				high = mid;
			else
				return midVal; // key found
		}
		null.asInstanceOf[T]; // key not found.
	}

	@inline
	def find[T <: AnyRef](sortedArray: Array[T])(criteria: FuncToInt[T]): T = {
		this.find(sortedArray, sortedArray.length)(criteria)
	}
}
