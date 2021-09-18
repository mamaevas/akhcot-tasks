package tochka.tasks.playground

import scala.util.control.TailCalls.{TailRec, done, tailcall}

object TreeNodeApp extends App {

  class TreeNode[X](val value: X, val left: Option[TreeNode[X]], val right: Option[TreeNode[X]]) {

    def canEqual(other: Any): Boolean = other.isInstanceOf[TreeNode[X]]

    override def equals(other: Any): Boolean = other match {
      case that: TreeNode[X] =>
        (that canEqual this) &&
          value == that.value &&
          left == that.left &&
          right == that.right
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(value, left, right)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  def isSameTreeEquals[X](p: Option[TreeNode[X]], q: Option[TreeNode[X]]): Boolean = p == q

  def isSameTreeRec[X](p: Option[TreeNode[X]], q: Option[TreeNode[X]]): Boolean = {
    (p, q) match {
      case (None, None)    => true
      case (Some(_), None) => false
      case (None, Some(_)) => false
      case (Some(p), Some(q)) =>
        p.value == q.value &&
          isSameTreeRec(p.left, q.left) &&
          isSameTreeRec(p.right, q.right)
    }
  }

  def isSameTreeRecStackSafe[X](p: Option[TreeNode[X]], q: Option[TreeNode[X]]): Boolean = {
    def isEqual(p: Option[TreeNode[X]], q: Option[TreeNode[X]]): TailRec[Boolean] = {
      if (p.isEmpty && q.isEmpty) done(true)
      else if (p.isEmpty && q.isDefined || p.isDefined && q.isEmpty) done(false)
      else {
        val _p = p.get
        val _q = q.get
        if (_p.value == _q.value) {
          for {
            res1 <- tailcall(isEqual(_p.left, _q.left))
            res2 <- tailcall(isEqual(_p.right, _q.right))
          } yield res1 && res2
        } else done(false)

      }
    }
    isEqual(p, q).result
  }

  val simpleP = new TreeNode(1, None, None)
  val simpleQ = new TreeNode(1, None, None)
  assert(isSameTreeEquals(Some(simpleP), Some(simpleQ)))
  assert(isSameTreeRec(Some(simpleP), Some(simpleQ)))
  assert(isSameTreeRecStackSafe(Some(simpleP), Some(simpleQ)))
  val p = new TreeNode(1, Some(new TreeNode(2, None, None)), None)
  val q = new TreeNode(1, None, Some(new TreeNode(2, None, None)))
  assert(!isSameTreeEquals(Some(p), Some(q)))
  assert(!isSameTreeRec(Some(p), Some(q)))
  assert(!isSameTreeRecStackSafe(Some(p), Some(q)))

  val TREE_SIZE_POWER = 20

  val (tmp1, size1) = (1 to TREE_SIZE_POWER).foldLeft(new TreeNode(0, None, None) -> 1L) {
    case ((acc, size), next) =>
      if (next == TREE_SIZE_POWER / 2) new TreeNode(-1, Some(acc), Some(acc)) -> (size * 2 + 1L)
      else new TreeNode(next, Some(acc), Some(acc)) -> (size * 2 + 1L)
  }

  val (tmp2, size2) = (1 to TREE_SIZE_POWER).foldLeft(new TreeNode(0, None, None) -> 1L) {
    case ((acc, size), next) => new TreeNode(next, Some(acc), Some(acc)) -> (size * 2 + 1L)
  }

  val (tmp3, size3) = (1 to TREE_SIZE_POWER).foldLeft(new TreeNode(0, None, None) -> 1L) {
    case ((acc, size), next) => new TreeNode(next, Some(acc), Some(acc)) -> (size * 2 + 1L)
  }

  def test(f: (Option[TreeNode[Int]], Option[TreeNode[Int]]) => Boolean, restarts: Int, name: String, printResult: Boolean) = {
    val start = System.currentTimeMillis()
    val res = (1 to restarts)
      .map(_ => !f(Some(tmp1), Some(tmp2)) && f(Some(tmp2), Some(tmp3)))
      .forall(identity)
    val end = System.currentTimeMillis()
    if (printResult)
      println(
        s"Result of test [$name]: ${if (res) "SUCCEEDED" else "FAILED"}; ${end - start} ms;   [ size1 = $size1; size2 = $size2; size3 = $size3 ]")
  }

  def testAll(restarts: Int, printResult: Boolean = false) = {
    test(isSameTreeEquals[Int], restarts, "isSameTree", printResult)
    test(isSameTreeRec[Int], restarts, "isSameTreeRec", printResult)
    test(isSameTreeRecStackSafe[Int], restarts, "isSameTreeRecStackSafe", printResult)
  }

  testAll(10) // preHeating
  testAll(10, true)

  /*
  Results with val TREE_SIZE_POWER = 20:
  Result of test [isSameTree]: SUCCEEDED; 165 ms;   [ size1 = 2097151; size2 = 2097151; size3 = 2097151 ]
  Result of test [isSameTreeRec]: SUCCEEDED; 165 ms;   [ size1 = 2097151; size2 = 2097151; size3 = 2097151 ]
  Result of test [isSameTreeRecStackSafe]: SUCCEEDED; 2691 ms;   [ size1 = 2097151; size2 = 2097151; size3 = 2097151 ]
 */
}
