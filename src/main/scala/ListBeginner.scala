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
    def loop(list: List[Int], reverseList: List[Int]): Boolean = list match {
      case Nil => false
      case x :: Nil => true
      case x :: xs => reverseList match {
        case Nil => false
        case y :: Nil => true
        case y :: ys => {
          if (x == y) loop(xs, ys)
          else false
        }
      }
    }
    loop(list, reverse(list))
  }

  /**
   * 7. リストの要素を，先頭から2つづつ並べてできるリストを返す関数 duplicate を作成して下さい
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate(list: List[Symbol]): List[Symbol] = {
    @tailrec
    def loop(list: List[Symbol], doubleList: List[Symbol]): List[Symbol] = list match {
      case Nil => Nil
      case x :: Nil => doubleList :+x :+x
      case x :: xs => loop(xs, doubleList :+ x :+ x)
    }
    loop(list, Nil)
  }

  /**
   * 8. リストを指定した位置で分割し，前半部分のリストと後半部分リストの組を返す関数 split を作成して下さい
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split(idx: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    @tailrec
    def loop(idx:Int, list: List[Symbol], pos: Int, splitList: List[Symbol]): (List[Symbol], List[Symbol]) = list match {
      case Nil => (Nil, Nil)
      case x :: Nil => (List(x), Nil)
      case x :: xs => {
        if (idx == pos) (splitList :+ x, xs)
        else loop(idx, xs, pos + 1, splitList :+ x)
      }
    }
    loop(idx, list, 1, Nil)
  }

  /**
   * 9. リストから，指定した位置の要素を除外して得られるリストと除外した要素の組を返す関数 removeAt を作成してください
   * 先頭の要素を 0 番目とします．
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt(idx: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    @tailrec
    def loop(idx: Int, list: List[Symbol], removedList: List[Symbol], pos: Int): (List[Symbol], Symbol) = list match {
      case Nil => (Nil, 'a)
      case x :: Nil => (removedList, x)
      case x :: xs => {
        if (idx == pos) (removedList ::: xs, x)
        else loop(idx, xs, removedList :+ x, pos + 1)
      }
    }
    loop(idx, list, Nil, 0)
  }

  /**
   * 10. リストの指定した位置に要素を挿入してできたリストを返す関数 insertAt を作成して下さい
   * 先頭の要素を 0 番目とします．
   * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt(newSymbol: Symbol, idx: Int, list: List[Symbol]): List[Symbol] = {
    @tailrec
    def loop(newSymbol: Symbol, idx: Int, list: List[Symbol], pos: Int, newList: List[Symbol]): List[Symbol] = list match {
      case Nil => Nil
      case x :: Nil => newList :+ x
      case x :: xs => {
        if (idx == pos) (newList :+ newSymbol :+ x) ::: xs
        else loop(newSymbol, idx, xs, pos + 1, newList :+ x)
      }
    }
    loop(newSymbol, idx, list, 0, Nil)
  }

  /**
   * 11. 2つの数 M, N (M <= N) に関して， M, M+1, ..., N-1 を並べたリストを返す関数 range を作成して下さい
   * scala> range(4, 9)
   * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range(from: Int, to: Int): List[Int] = {
    @tailrec
    def loop(from: Int, width: Int, idx: Int, seqList: List[Int]): List[Int] = width match {
      case 0 => seqList :+ from
      case _ => {
        if (width < idx) (seqList :+ from + idx) :+ (from + idx + 1) :+ (from + width + idx)
        else loop(from, width - 1, idx + 1, seqList :+ from + idx)
      }
    }
    loop(from, to - from, 0, Nil)
  }

}
