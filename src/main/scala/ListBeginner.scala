import scala.annotation.tailrec

/**
 * Created by shibuekazuaki on 15/11/12.
 */
object ListBeginner {

  /**
   *
   * 1. リストの最後の要素を返す関数 last を作成して下さい
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   *
   */
  @tailrec
  def last(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case x :: Nil => x
      case x :: xs => last(xs)
    }
  }

  /**
   * 2. リストの最後から2番目の要素を返す関数 penultimate を作成して下さい
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  @tailrec
  def penultimate(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case x :: Nil => x
      case x :: xs => {
        if (xs.size == 1) x
        else penultimate(xs)
      }
    }
  }

  /**
   * 3. リストの n 番目の要素を返す関数 nth を作成して下さい
   * 最初の要素を0番目とします
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  def nth(idx: Int, list: List[Int]): Int = {
    @tailrec
    def loop(idx: Int, list: List[Int], acc: Int): Int = list match {
      case Nil => 0
      case x :: Nil => x
      case x :: xs => {
        if (idx == acc) x
        else loop(idx, xs, acc + 1)
      }
    }
    loop(idx, list, 0)
  }

  /**
   * 4. リストの長さを返す関数 length を作成して下さい
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length(list: List[Int]): BigInt = {
    @tailrec
    def loop(list: List[Int], acc: BigInt): BigInt = list match {
      case Nil => acc
      case x :: Nil => acc + 1
      case x :: xs => loop(xs, acc + 1)
    }
    loop(list, 0)
  }

  /**
   * 5. リストを逆順に並べてできるリストを返す関数 reverse を作成して下さい
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse(list: List[Int]): List[Int] = {
    @tailrec
    def loop(list: List[Int], acc: List[Int]): List[Int] = list match {
      case Nil => acc
      case x :: Nil => x :: acc
      case x :: xs => loop(xs, x :: acc)
    }
    loop(list, Nil)
  }

  /**
   * 6. リストが回文になっている場合真を，そうでない場合に偽を返す関数 isPalindrome を作成して下さい
   * 先頭から読んでも末尾から読んでも同じ順番で要素が並んでいる場合，リストが回文であるとします．
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome(list: List[Int]): Boolean = {
    def loop(list: List[Int], left: Int) = list match {
      case Nil => false
      case x :: Nil => true
      case x :: xs
    }
  }

  /**
   * 7. リストの要素を，先頭から2つづつ並べてできるリストを返す関数 duplicate を作成して下さい
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate(list: List[Symbol]): List[Symbol] = {
    ???
  }

  /**
   * 8. リストを指定した位置で分割し，前半部分のリストと後半部分リストの組を返す関数 split を作成して下さい
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split() = {
    ???
  }

  /**
   * 9. リストから，指定した位置の要素を除外して得られるリストと除外した要素の組を返す関数 removeAt を作成してください
   * 先頭の要素を 0 番目とします．
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt() = {
    ???
  }

  /**
   * 10. リストの指定した位置に要素を挿入してできたリストを返す関数 insertAt を作成して下さい
   * 先頭の要素を 0 番目とします．
   * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt() = {
    ???
  }

  /**
   * 11. 2つの数 M, N (M <= N) に関して， M, M+1, ..., N-1 を並べたリストを返す関数 range を作成して下さい
   * scala> range(4, 9)
   * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range() = {
    ???
  }

}
