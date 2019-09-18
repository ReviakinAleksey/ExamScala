import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class Exam1Spec extends FlatSpec {
  it should "not produce State for negative opened" in {
    Exam1.produceValidState(-1, 1, "(") shouldBe empty
  }

  it should "not produce State when no space lest" in {
    Exam1.produceValidState(1, 2, "((") shouldBe empty
  }

  it should "produce State for corrent values" in {
    Exam1.produceValidState(5, 1, "(") should equal(List(Exam1.State(1, "(")))
  }

  it should "calculate solution for 0" in {
    Exam1.solution(0) should equal(Nil)
  }

  it should "calculate solution for 1" in {
    Exam1.solution(1) should equal(List("()"))
  }

  it should "calculate solution for 2" in {
    Exam1.solution(2) should equal(List("()()", "(())"))
  }
}
