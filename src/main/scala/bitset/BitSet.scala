package bitset

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class BitSet {
  private val ar: ArrayBuffer[Long] = ArrayBuffer()

  private def coords(i: Int): (Int, Int) = (i / 64, i % 64)

  private def expand(i: Int): Unit = {
    val j = i - (ar.size - 1)
    for (_ <- 0 until j) ar.append(0)
  }

  def set(v: Int): Unit = {
    val (index, offset) = coords(v)
    expand(index)
    ar(index) = ar(index) | (1L << offset)
  }

  def get(v: Int): Boolean = {
    val (index, offset) = coords(v)
    if (index >= 0 && index < ar.size) {
      val result = ar(index) & (1L << offset)
      result != 0
    } else {
      false
    }
  }

  def clear(v: Int): Unit = {
    val (index, offset) = coords(v)
    ar(index) = ar(index) & (~(1L << offset))
  }

  def and(s: BitSet): Unit =
    for (i <- ar.indices) {
      val v = if (s.ar.isDefinedAt(i)) s.ar(i) else 0
      ar(i) = ar(i) & v
    }

  def or(s: BitSet): Unit = {
    expand(s.ar.size)
    for (i <- s.ar.indices)
      ar(i) = ar(i) | s.ar(i)
  }

  def xor(s: BitSet): Unit = {
    expand(s.ar.size)
    for (i <- s.ar.indices)
      ar(i) = ar(i) ^ s.ar(i)
  }

  def flip(v: Int): Unit = {
    val (index, offset) = coords(v)
    expand(index)
    ar(index) = ar(index) ^ (1L << offset)
  }

  def intersects(s: BitSet): Boolean = {
    val min = Math.min(ar.size, s.ar.size)
    @tailrec
    def loop(i: Int): Boolean =
      if (i >= min) {
        false
      } else if ((ar(i) & s.ar(i)) != 0) {
        true
      } else {
        loop(i + 1)
      }
    loop(0)
  }

  def toLongArray: Array[Long] = ar.toArray.clone()

  override def toString: String = {
    ar.toString()
  }
}