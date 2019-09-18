import org.scalatest.FlatSpec

class Exam2Spec extends FlatSpec {

  it should "calculate rootDepth for 16" in {
    assert(Exam2.rootDepth(16) == 2)
  }
  it should "calculate rootDepth for 6561" in {
    assert(Exam2.rootDepth(6561) == 3)
  }

  it should "calculate solution for 10 20" in {
    assert(Exam2.solution(10, 20) == 2)
  }

  it should "calculate solution for 6000 7000" in {
    assert(Exam2.solution(6000, 7000) == 3)
  }
}
