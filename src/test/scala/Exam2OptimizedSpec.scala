import Exam2Optimized.IntSqrt
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class Exam2OptimizedSpec extends FlatSpec {

  it should "findSqrtIntOffset for 9" in {
    Exam2Optimized.findSqrtIntOffset(9) should equal(IntSqrt(3, 0, 7))
  }

  it should "findSqrtIntOffset for 6561" in {
    Exam2Optimized.findSqrtIntOffset(6561) should equal(IntSqrt(81, 0, 163))
  }

  it should "findSqrtIntOffset for 10" in {
    Exam2Optimized.findSqrtIntOffset(10) should equal(IntSqrt(4, 6, 9))
  }

  it should "findSqrtDepth for non root values" in {
    Exam2Optimized.findSqrtDepth(2, 0) should equal(0)
  }

  it should "findSqrtDepth for root values" in {
    Exam2Optimized.findSqrtDepth(6561, 0) should equal(3)
  }

  it should "calculate solution for 10 20" in {
    assert(Exam2Optimized.solution(10, 20) == 2)
  }

  it should "calculate solution for 6000 7000" in {
    assert(Exam2Optimized.solution(6000, 7000) == 3)
  }

}
