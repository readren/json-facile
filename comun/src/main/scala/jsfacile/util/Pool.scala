package jsfacile.util

import scala.collection.mutable

import jsfacile.util.Pool.{NO_ONE, UserId}

object Pool {
	type UserId = Int
	val NO_ONE: UserId = 0; // should be even;
}

/** CAUTION: not thread safe */
class Pool[T <: AnyRef] {

	trait Allocator {
		def alloc: T
	}

	class Overseer(val elem: T) {
		@volatile var takenBy: UserId = 0;
	}

	private var userIdSequencer: UserId = 1; // should be initialized with an odd number;

	def registerUser(): UserId = {
		userIdSequencer += 2; // all valid user ids are odd
		userIdSequencer
	}

	def unregisterUser(userId: UserId): Unit = {
		var i = overseers.size;
		while (i > 0) {
			i -= 1;
			val m = overseers(i);
			if(m.takenBy == userId) {
				m.takenBy = NO_ONE
			}
		}

	}

	val overseers: mutable.ArrayBuffer[this.Overseer] = mutable.ArrayBuffer.empty

	def borrow(userId: UserId)(implicit allocator: Allocator): T = {
		// find an available element
		var i = overseers.size;
		do i -= 1;
		while (i >= 0 && overseers(i).takenBy != NO_ONE)

		val overseer = if (i >= 0) { // if an element is available, mark is as under use and give it
			overseers(i);
		} else { // if no element is available, create one, add it to the buffer, and return it.
			val elem = allocator.alloc;
			val m = new Overseer(elem);
			overseers.addOne(m);
			m
		}
		overseer.takenBy = userId;
		overseer.elem
	}

	def giveBack(elem: T): Unit = {
		var i = overseers.size;
		do i -= 1;
		while (i >= 0 && (overseers(i).elem ne elem))
		if (i >= 0) {
			overseers(i).takenBy = NO_ONE
		}
	}

}

