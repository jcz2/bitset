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
        val bsq = new BitSet()
        val bs = new util.BitSet()

        forAll(gen, minSuccessful(1000)) { v =>
          bsq.set(v)
          bs.set(v)
          bsq.get(v) shouldEqual bs.get(v)
          bsq.toLongArray shouldEqual bs.toLongArray
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
          val bsq1 = new BitSet()
          val bsq2 = new BitSet()

          l1.foreach(v => {
            bs1.set(v)
            bsq1.set(v)
          })

          l2.foreach(v => {
            bs2.set(v)
            bsq2.set(v)
          })

          bs1.and(bs2)
          bsq1.and(bsq2)

          bs1.toLongArray.sum shouldEqual bsq1.toLongArray.sum
          bs2.toLongArray.sum shouldEqual bsq2.toLongArray.sum
        }
      }
    }

    describe("or") {
      it("should return proper value") {
        forAll(listPairGen, minSuccessful(1000)) { case (l1, l2) =>
          val bs1 = new util.BitSet()
          val bs2 = new util.BitSet()
          val bsq1 = new BitSet()
          val bsq2 = new BitSet()

          l1.foreach(v => {
            bs1.set(v)
            bsq1.set(v)
          })

          l2.foreach(v => {
            bs2.set(v)
            bsq2.set(v)
          })

          bs1.or(bs2)
          bsq1.or(bsq2)

          bs1.toLongArray.sum shouldEqual bsq1.toLongArray.sum
          bs2.toLongArray.sum shouldEqual bsq2.toLongArray.sum
        }
      }
    }

    describe("flip") {
      it("should return proper value") {
        val bsq = new BitSet()
        val bs = new util.BitSet()

        forAll(gen, minSuccessful(1000)) { v =>
          bsq.flip(v)
          bs.flip(v)
          bsq.toLongArray shouldEqual bs.toLongArray
        }
      }
    }

    describe("clear") {
      it("should return proper value") {
        val bsq = new BitSet()
        val bs = new util.BitSet()

        Gen.listOfN(1000, gen).sample.foreach(_.foreach(v => {
          bsq.set(v)
          bs.set(v)
        }))

        forAll(gen, minSuccessful(1000)) { v =>
          bsq.clear(v)
          bs.clear(v)
          bsq.toLongArray shouldEqual bs.toLongArray
        }
      }
    }

    describe("intersects") {
      it("should return proper value") {
        val bs1 = new util.BitSet()
        val bs2 = new util.BitSet()
        val bsq1 = new BitSet()
        val bsq2 = new BitSet()
        val pairGen = for (i <- gen; j <- gen) yield (i, j)

        forAll(pairGen, minSuccessful(1000)) { case (l1, l2) =>
          bs1.set(l1)
          bsq1.set(l1)

          bs2.set(l2)
          bsq2.set(l2)

          bsq1.intersects(bsq2) shouldEqual bs1.intersects(bs2)
        }
      }
    }
  }
}
