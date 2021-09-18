package tochka.tasks.playground

import scala.collection.immutable.NumericRange
import scala.util.Random

object SortStringApp extends App {

  implicit class StringExt(str: String) {
    def sortedAndGroupedByChar(implicit chOrdering: Ordering[(Char, String)]): String =
      str
        .groupBy(identity)
        .toSeq
        .sorted(chOrdering)
        .map(_._2)
        .mkString
  }

  implicit val chOrdering: Ordering[(Char, String)] =
    Ordering.by[(Char, String), Int](_._2.length).reverse

  val stableChOrdering: Ordering[(Char, String)] =
    (x: (Char, String), y: (Char, String)) => if (x._2.length == y._2.length) x._1.compare(y._1) else y._2.length.compare(x._2.length)

  val testString = "asdasdaaaweqbbbbasdasd"

// группы с одинаковым кол-вом символов могут быть в произвольном порядке, например “qwe” или “eqw”
  assert(testString.sortedAndGroupedByChar == "aaaaaaassssbbbbddddeqw")

// учитываем оба параметра при сортировке (порядковый номер символа и количество символов), поэтому тест стабилен
  assert(testString.sortedAndGroupedByChar(stableChOrdering) == "aaaaaaabbbbddddsssseqw")
  assert("abcaba".sortedAndGroupedByChar(stableChOrdering) == "aaabbc")
  assert("aacbc".sortedAndGroupedByChar(stableChOrdering) == "aaccb")

  def stringGenerator(maxGroupSize: Int, characterRange: NumericRange[Char]) = {
    characterRange
      .foldLeft(new StringBuilder) { (sb, ch) =>
        sb ++= (1 to (Random.nextInt(maxGroupSize) + 1)).foldLeft(new StringBuilder)((_sb, _) => _sb += ch)
      }
  }

  def largeStringGenerator(factor: Int, maxGroupSize: Int, characterRange: NumericRange[Char]) = {
    (1 to factor).foldLeft(new StringBuilder)((sb, _) => sb ++= stringGenerator(maxGroupSize, characterRange))
  }

  // 100M string
  val generatedLargeString = largeStringGenerator(4 * 1000, 1000, ('a' to 'z')).mkString

  def test(str: => String, testName: String): Unit = {
    val start = System.currentTimeMillis()
    str
    val end = System.currentTimeMillis()
    println(s"${testName}: string size = ${str.length}; time = ${end - start} ms.")
  }

  test(generatedLargeString.sortedAndGroupedByChar, "preHeat")
  test(generatedLargeString.sortedAndGroupedByChar, "sortedAndGroupedByChar")
  test(generatedLargeString.sortedAndGroupedByChar(stableChOrdering), "sortedAndGroupedByChar(stableOrdering)")

  /* Results
  preHeat: string size = 52127894; time = 1993 ms.
  sortedAndGroupedByChar: string size = 52127894; time = 1441 ms.
  sortedAndGroupedByChar(stableOrdering): string size = 52127894; time = 1278 ms.
 */
}
