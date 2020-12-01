package jsfacile.util

import java.util

import jsfacile.util.BitSet.{BitSlot, Shard}

object BitSet {
	type Shard = Long;

	final case class BitSlot(shardMask: Shard, shardIndex: Int) {
		def shifted: BitSlot = {
			val mask = this.shardMask << 1;
			if (mask == 0) BitSlot(1L, this.shardIndex + 1)
			else BitSlot(mask, this.shardIndex)
		}
	}

	val FIRST_BIT: BitSlot = BitSlot(1L, 0);

	def empty(capacity: Int = 0) = new BitSet(new Array(capacity));
}

/** A mutable bit set. */
final class BitSet(val shards: Array[Shard]) extends AnyVal {

	def clear(): Unit = {
		var index = shards.length;
		while (index > 0) {
			index -= 1;
			shards(index) = 0L;
		}
	}

	def |=(bit: BitSlot): Unit = {
		this.shards(bit.shardIndex) |= bit.shardMask;
	}

	/** Adds the received bit to this bit set.
	 * @return this instance with the bit added if it was possible to add in place, else a copy of this [[BitSet]] with the bit added. */
	def add(bit: BitSlot): BitSet = {
		if (bit.shardIndex < this.shards.length) {
			this |= bit
			this

		} else {
			val newShards = new Array[Shard](bit.shardIndex + 1);
			System.arraycopy(this.shards, 0, newShards, 0, this.shards.length);
			newShards(bit.shardIndex) = bit.shardMask;
			new BitSet(newShards)
		}
	}

	def isSubsetOf(other: BitSet): Boolean = {
		var yes = true;
		var index = shards.length;
		do index -= 1
		while (index >= 0 && (this.shards(index) & other.shards(index)) == this.shards(index));
		index < 0;
	}

}
