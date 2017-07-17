package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    if (chars.nonEmpty && chars.contains('(')) {
      //first find last opening sign
      val endingChars: Array[Char] = chars.slice(chars.lastIndexOf('(')+1,chars.length)
      if (endingChars.nonEmpty && endingChars.contains(')')) {
        val remainingChars: Array[Char] = endingChars.slice(endingChars.indexOf(')')+1, endingChars.length)

        val startingChars: Array[Char] = chars.slice(0,chars.lastIndexOf('('))//know where is that last element
        balance(startingChars ++ remainingChars)
        //from that place find the first closing bracket
        //rerun on the list without brackets and elements between
      } else {
        false
      }

    } else {
      !chars.contains(')')
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx < until) {
        chars(idx) match {
          case _ => traverse(idx + 1, until, arg1, arg2)
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2 - 1)
          case ')' => traverse(idx + 1, until, arg1 - 1, arg2 + 1)
        }
      }
      else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from > threshold) {
        val separator = (from + until) / 2
        val ((first1, first2), (second1, second2)) =
          parallel(reduce(from, separator),
                   reduce(separator, until))

        (first1 + second1, first2 + second2)
      }
      else {
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
