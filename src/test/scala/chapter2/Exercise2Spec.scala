package chapter2

import org.scalatest.{FlatSpec, Matchers}
import chapter2.Exercise2._
class Exercise2Spec extends FlatSpec with Matchers {

  "Fibonacci of 1 "should "be 1  " in {
    fibonnacci(1) shouldBe 1
  }

  "Fibonacci of 4 "should "be 1  " in {
    fibonnacci(4) shouldBe 3
  }

  "Fibonacci of 10 "should "be 55  " in {
    fibonnacci(10) shouldBe 55
  }

  "Fibonacci of 15 "should "be 610  " in {
    fibonnacci(15) shouldBe 610
  }
}
