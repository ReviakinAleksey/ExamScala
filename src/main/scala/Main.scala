import scala.annotation.tailrec

object Exam1 {

  case class State(opened: Int, value: String)

  def produceValidState(charsLeft: Int, opened: Int, value: String): List[State] = {
    if (opened >= 0 && charsLeft >= opened) {
      List(State(opened, value))
    } else {
      Nil
    }
  }

  @tailrec
  def bracketsFill(states: List[State], charsLeft: Int): List[State] = {
    if (charsLeft == 0) {
      states
    } else {
      bracketsFill(
        states = states.flatMap(s => {
          produceValidState(charsLeft, s.opened + 1, s.value + "(") ++ produceValidState(charsLeft, s.opened - 1, s.value + ")")
        }),
        charsLeft = charsLeft - 1)
    }
  }

  def solution(bracketsCount: Int): List[String] = {
    if (bracketsCount <= 0) {
      return Nil
    }
    bracketsFill(List(State(1, "(")), (bracketsCount * 2) - 1)
      .map(_.value)
      .sorted
      .reverse
  }

}

object Exam2Optimized {

  case class IntSqrt(nearestSqrt: Int, offsetToSqrt: Int, offsetToNextSqrt: Int)

  case class SqrtData(intSqrt: Int, number: Int, offsetToNextSqrt: Int)

  def findSqrtIntOffset(n: Int): IntSqrt = {
    LazyList
      .iterate(IntSqrt(0, n, 1))(i => i.copy(i.nearestSqrt + 1, i.offsetToSqrt - i.offsetToNextSqrt, i.offsetToNextSqrt + 2))
      .collectFirst({
        case IntSqrt(nearestSqrt, offsetToSqrt, offsetToNextSqrt) if offsetToSqrt <= 0 =>
          IntSqrt(nearestSqrt, -offsetToSqrt, offsetToNextSqrt)
      })
      .get
  }

  @tailrec
  def findSqrtDepth(n: Int, depth: Int): Int = {
    Exam2Optimized.findSqrtIntOffset(n) match {
      case IntSqrt(nextSqrt, 0, _) =>
        findSqrtDepth(nextSqrt, depth + 1)
      case IntSqrt(_, _, _) =>
        depth
    }
  }

  def solution(start: Int, end: Int): Int = {
    val IntSqrt(firstSqrtValue, offsetToSqrt, offsetToNextSqrt) = findSqrtIntOffset(start)
    val firstHasSqrt = start + offsetToSqrt

    val initial = SqrtData(firstSqrtValue, firstHasSqrt, offsetToNextSqrt)
    LazyList.iterate(initial)(i => i.copy(i.intSqrt + 1, i.number + i.offsetToNextSqrt, i.offsetToNextSqrt + 2))
      .takeWhile(_.number <= end)
      .map(data => findSqrtDepth(data.intSqrt, 1))
      .max
  }
}

object Exam2 {
  @tailrec
  def rootDepth(n: Int, currentDepth: Int = 0): Int = {
    val sqrt = math.sqrt(n)
    val sqrtInt = sqrt.toInt
    if (sqrtInt != sqrt) {
      return currentDepth
    }
    rootDepth(sqrtInt, currentDepth + 1)
  }

  def solution(start: Int, end: Int): Int = {
    LazyList.from(start).take(end - start + 1)
      .map(rootDepth(_))
      .max
  }
}


object Exam3 {

  val SIZE_TABLE: Array[Int] = Array(9, 99, 999, 9999, 99999, 999999, 9999999, 99999999, 999999999, Integer.MAX_VALUE)

  // Inspired by from http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/687fd7c7986d/src/share/classes/java/lang/Integer.java
  def zerosCount(n: Int): Int = {
    assert(n > 0)
    LazyList.from(0)
      .find(i => n <= SIZE_TABLE(i))
      .getOrElse(throw new IllegalStateException("Should newer happen"))
  }

  @tailrec
  def intPow(power: Int, result: Int = 1): Int = {
    power match {
      case 0 =>
        result
      case _ =>
        intPow(power - 1, result * 10)
    }
  }

  def solution(number: Int): Int = {
    val numberSize = zerosCount(number)
    if (numberSize == 0) {
      0
    } else {
      intPow(numberSize)
    }
  }
}

object Main extends App {
  println("Exam1")
  Exam1.solution(3).foreach(println)
  println("=======================")
  println("Exam2")
  println(Exam2.solution(6000, 7000))
  println("=======================")
  println("Exam2Optimized")
  println(Exam2Optimized.solution(2, 1_000_000_000))
  println("=======================")
  println("Exam3")
  println(Exam3.solution(243))
  println("=======================")
}
