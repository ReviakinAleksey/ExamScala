import org.scalatest.FlatSpec

class Exam3Spec extends FlatSpec {
  it should "calculate zerosCount for 100" in {
    assert(Exam3.zerosCount(100) == 2)
  }

  it should "calculate zerosCount for 1" in {
    assert(Exam3.zerosCount(1) == 0)
  }

  it should "calculate intPower for 0" in {
    assert(Exam3.intPow(0) == 1)
  }

  it should "calculate intPower for 3" in {
    assert(Exam3.intPow(3) == 1000)
  }

  it should "calculate solution for 1" in {
    assert(Exam3.solution(1) == 0)
  }

  it should "calculate solution for 10" in {
    assert(Exam3.solution(10) == 10)
  }

  it should "calculate solution for 143" in {
    assert(Exam3.solution(143) == 100)
  }
}