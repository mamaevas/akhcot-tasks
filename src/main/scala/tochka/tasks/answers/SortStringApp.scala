package tochka.tasks.answers

/**
  * Task 1
  * Есть строка, нужно используя методы коллекций скалы, сгруппировать буквы по символу,
  * отсортировать группы по количеству символов и сджойнить группы в одну строку.
  * Пример: “abcaba” -> “aaabbc” или “aacbc” -> “aaccb”
  */
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

  val testString = "asdasdaaaweqbbbbasdasd"

  // группы с одинаковым кол-вом символов могут быть в произвольном порядке, например “qwe” или “eqw”
  assert(testString.sortedAndGroupedByChar == "aaaaaaassssbbbbddddeqw")
}
