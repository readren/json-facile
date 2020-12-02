package jsfacile.util

import java.lang.ref.WeakReference

import scala.collection.mutable

import jsfacile.util.Pool.{Allocator, NO_ONE, UserId}

object Pool {
	type UserId = Int
	val NO_ONE: UserId = 0; // should be even;

	trait Allocator[T] {
		def alloc: T
	}
}

/** CAUTION: not thread safe */
class Pool[T <: AnyRef] {

	class Overseer(val elem: T) {
		var takenBy: UserId = 0;
	}

	private var userIdSequencer: UserId = 1; // should be initialized with an odd number;

	@inline def registerUser(): UserId = {
		userIdSequencer += 2; // all valid user ids are odd
		userIdSequencer
	}

	def unregisterUser(userId: UserId): Unit = {
		var i = overseersBufferSize;
		while (i > 0) {
			i -= 1;
			val overseer = overseersBuffer(i).get;
			if (overseer != null && overseer.takenBy == userId) {
				overseer.takenBy = NO_ONE
			}
		}

	}

	/** The buffer where [[Overseer]] instances are stored.
	 * The [[WeakReference]]s are necessary to tolerate the forgetfulness of [[giveBack]] call by the user. And also avoids a requirement to clean the [[Overseer]] when the execution ends. */
	private val overseersBuffer: mutable.ArrayBuffer[WeakReference[this.Overseer]] = mutable.ArrayBuffer.empty;
	/** The size of the [[overseersBuffer]]. Used instead of [[overseersBuffer.size]] because the [[mutable.ArrayBuffer]] has no efficient way to reduce its size. And I didn't find a better mutable indexed collection. */
	private var overseersBufferSize: Int = 0;

	/** Moves the last non garbage collected [[Overseer]] of the [[overseersBuffer]] to the specified index, and adjust the [[overseersBufferSize]]. */
	private def moveLastOverseerToIndex(newIndex: Int): Overseer = {
		var lastOverseer: Overseer = null;

		var i = this.overseersBufferSize;
		do i -= 1
		while (i > newIndex && {
			this.overseersBufferSize = i;
			lastOverseer = this.overseersBuffer(i).get;
			if (lastOverseer == null) {
				this.overseersBuffer(i) = null;
				true // continue the search
			} else {
				this.overseersBuffer.update(newIndex, this.overseersBuffer(i))
				this.overseersBuffer(i) = null;
				false
			}
		})
		lastOverseer
	}

	def borrow(userId: UserId)(implicit allocator: Allocator[T]): Overseer = {
		var overseer: Overseer = null;
		var found: Boolean = false;

		// find an available overseer
		var i = 0;
		while (i < this.overseersBufferSize && {
			overseer = overseersBuffer(i).get;
			// if the overseer was garbage collected, fill the hole moving the last overseer of the `overseersBuffer` here.
			if (overseer == null) {
				overseer = moveLastOverseerToIndex(i)
			}
			found = overseer != null && overseer.takenBy == NO_ONE;
			!found
		}) i += 1;

		if (!found) { // if no overseer is available, create one, add it to the buffer, and return it.
			val elem = allocator.alloc;
			overseer = new Overseer(elem);
			if (overseersBufferSize < overseersBuffer.size) {
				overseersBuffer.update(overseersBufferSize, new WeakReference[Overseer](overseer));
			} else {
				overseersBuffer.addOne(new WeakReference(overseer));
			}
			overseersBufferSize += 1;
		}
		overseer.takenBy = userId;
		overseer
	}

	@inline def giveBack(overseer: Overseer): Unit = {
		overseer.takenBy = NO_ONE
	}

	def borrowInsideOf[R](body: T => R)(implicit allocator: Allocator[T]): R = {
		val userId = registerUser();
		val overseer = borrow(userId)
		val r = body(overseer.elem);
		giveBack(overseer);
		r
	}

}

