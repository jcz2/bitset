import bitset.BitSet
import org.scalacheck._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util

class BitSetSpec extends AnyFunSpec with ScalaCheckPropertyChecks with Matchers with Configuration{

  describe("BitSet") {
    val gen = Gen.choose(0, 1000)
    val listGen = for (len <- gen; l <- Gen.listOfN(len, gen)) yield l
    val listPairGen = for (l1 <- listGen; l2 <- listGen) yield (l1, l2)

    describe("get") {
      it("should return proper value") {
        val bs_ = new BitSet()
        val bs = new util.BitSet()

        forAll(gen, minSuccessful(1000)) { v =>
          bs_.set(v)
          bs.set(v)
          bs_.get(v) shouldEqual bs.get(v)
          bs_.toLongArray shouldEqual bs.toLongArray
        }
      }
    }

    describe("and") {
      it("should return proper value") {
        val listGen = for (len <- gen; l <- Gen.listOfN(len, gen)) yield l
        val pairGen = for (l1 <- listGen; l2 <- listGen) yield (l1, l2)

        forAll(pairGen, minSuccessful(1000)) { case (l1, l2) =>
          val bs1 = new util.BitSet()
          val bs2 = new util.BitSet()
          val bs_1 = new BitSet()
          val bs_2 = new BitSet()

          l1.foreach(v => {
            bs1.set(v)
            bs_1.set(v)
          })

          l2.foreach(v => {
            bs2.set(v)
            bs_2.set(v)
          })

          bs1.and(bs2)
          bs_1.and(bs_2)

          bs1.toLongArray.sum shouldEqual bs_1.toLongArray.sum
          bs2.toLongArray.sum shouldEqual bs_2.toLongArray.sum
        }
      }
    }

    describe("or") {
      it("should return proper value") {
        forAll(listPairGen, minSuccessful(1000)) { case (l1, l2) =>
          val bs1 = new util.BitSet()
          val bs2 = new util.BitSet()
          val bs_1 = new BitSet()
          val bs_2 = new BitSet()

          l1.foreach(v => {
            bs1.set(v)
            bs_1.set(v)
          })

          l2.foreach(v => {
            bs2.set(v)
            bs_2.set(v)
          })

          bs1.or(bs2)
          bs_1.or(bs_2)

          bs1.toLongArray.sum shouldEqual bs_1.toLongArray.sum
          bs2.toLongArray.sum shouldEqual bs_2.toLongArray.sum
        }
      }
    }

    describe("flip") {
      it("should return proper value") {
        val bs_ = new BitSet()
        val bs = new util.BitSet()

        forAll(gen, minSuccessful(1000)) { v =>
          bs_.flip(v)
          bs.flip(v)
          bs_.toLongArray shouldEqual bs.toLongArray
        }
      }
    }

    describe("clear") {
      it("should return proper value") {
        val bs_ = new BitSet()
        val bs = new util.BitSet()

        Gen.listOfN(1000, gen).sample.foreach(_.foreach(v => {
          bs_.set(v)
          bs.set(v)
        }))

        forAll(gen, minSuccessful(1000)) { v =>
          bs_.clear(v)
          bs.clear(v)
          bs_.toLongArray shouldEqual bs.toLongArray
        }
      }
    }

    describe("intersects") {
      it("should return proper value") {
        val bs1 = new util.BitSet()
        val bs2 = new util.BitSet()
        val bs_1 = new BitSet()
        val bs_2 = new BitSet()
        val pairGen = for (i <- gen; j <- gen) yield (i, j)

        forAll(pairGen, minSuccessful(1000)) { case (l1, l2) =>
          bs1.set(l1)
          bs_1.set(l1)

          bs2.set(l2)
          bs_2.set(l2)

          bs_1.intersects(bs_2) shouldEqual bs1.intersects(bs2)
        }
      }
    }

    describe("xor") {
      it("should return proper value") {
        forAll(listPairGen, minSuccessful(1000)) { case (l1, l2) =>
          val bs1 = new util.BitSet()
          val bs2 = new util.BitSet()
          val bs_1 = new BitSet()
          val bs_2 = new BitSet()

          l1.foreach(v => {
            bs1.set(v)
            bs_1.set(v)
          })

          l2.foreach(v => {
            bs2.set(v)
            bs_2.set(v)
          })

          bs1.xor(bs2)
          bs_1.xor(bs_2)

          bs1.toLongArray.sum shouldEqual bs_1.toLongArray.sum
          bs2.toLongArray.sum shouldEqual bs_2.toLongArray.sum
        }
      }
    }
  }
}
