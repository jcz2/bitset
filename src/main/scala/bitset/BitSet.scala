package bitset

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class BitSet {
  private val ar: ArrayBuffer[Long] = ArrayBuffer()

  private def offset(i: Int): (Int, Int) = (i / 64, i % 64)

  private def expand(i: Int): Unit = {
    if (i >= ar.size) {
      var d = i - ar.size + 1
      while (d > 0) {
        ar.append(0)
        d -= 1
      }
    }
  }

  def set(v: Int): Unit = {
    val (i, n) = offset(v)
    expand(i)
    ar(i) = ar(i) | (1L << n)
  }

  def get(v: Int): Boolean = {
    val (i, n) = offset(v)
    val result = ar(i) & (1L << n)
    result != 0
  }

  def clear(v: Int): Unit = {
    val (i, n) = offset(v)
    ar(i) = ar(i) & (~(1L << n))
  }

  def cardinality(): Int = {
    ar.foldLeft(0)((acc, v) => acc + java.lang.Long.bitCount(v))
  }

  def and(s: BitSet): Unit = {
    if (ar.size > s.ar.size) {
      for (i <- s.ar.size until ar.size) {
        ar(i) = 0L
      }
    }
    val min = Math.min(ar.size, s.ar.size)
    for (i <- 0 until min) {
      ar(i) = ar(i) & s.ar(i)
    }
  }

  def or(s: BitSet): Unit = {
    if (ar.size < s.ar.size) {
      for (i <- ar.indices) {
        ar(i) = ar(i) | s.ar(i)
      }
      for (i <- ar.size until s.ar.size) {
        ar.append(s.ar(i))
      }
    } else {
      for (i <- s.ar.indices) {
        ar(i) = ar(i) | s.ar(i)
      }
    }
  }

  def flip(v: Int): Unit = {
    val (i, n) = offset(v)
    expand(i)
    ar(i) = ar(i) ^ (1L << n)
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