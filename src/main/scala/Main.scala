import scala.annotation.tailrec
enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])
  def reverse: MyList[A] =
    @tailrec
    def go[A](xs: MyList[A], result: MyList[A] = MyNil): MyList[A] =
      xs match
        case MyNil          => result
        case MyCons(hd, tl) => go(tl, MyCons(hd, result))
    go(this)

import MyList.*

object MyList {
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }
}

def takeFirst[A](xs: MyList[A], n: Int): MyList[A] =
  @tailrec
  def go[A](xs: MyList[A], n: Int, result: MyList[A] = MyNil): MyList[A] =
    xs match
      case MyNil => result.reverse
      case MyCons(hd, tl) =>
        if n > 0 then go(tl, n - 1, MyCons(hd, result)) else result.reverse
  go(xs, n)

def takeWhile[A](xs: MyList[A], p: A => Boolean): MyList[A] =
  @tailrec
  def go[A](
      xs: MyList[A],
      p: A => Boolean,
      result: MyList[A] = MyNil
  ): MyList[A] =
    xs match
      case MyNil => result.reverse
      case MyCons(hd, tl) =>
        if p(hd) then go(tl, p, MyCons(hd, result)) else result.reverse
  go(xs, p)

def indexOf[A](xs: MyList[A], a: A): Option[Int] =
  @tailrec
  def go[A](xs: MyList[A], a: A, index: Int = 0): Option[Int] =
    xs match
      case MyNil => None
      case MyCons(hd, tl) =>
        if hd == a then Some(index)
        else go(tl, a, index + 1)
  go(xs, a)

def indexOfNth[A](xs: MyList[A], a: A, position: Int): Option[Int] =
  @tailrec
  def go[A](xs: MyList[A], a: A, position: Int, index: Int = 0): Option[Int] =
    xs match
      case MyNil => None
      case MyCons(hd, tl) =>
        if hd == a then
          position match
            case 1 => Some(index)
            case _ => go(tl, a, position - 1, index + 1)
        else go(tl, a, position, index + 1)
  go(xs, a, position)

def unfold[A](a: A)(next: A => Option[A]): MyList[A] =
  @tailrec
  def go[A](a: A, result: MyList[A])(next: A => Option[A]): MyList[A] =
    next(a) match
      case None    => result.reverse
      case Some(i) => go(i, MyCons(a, result))(next)
  go(a, MyNil)(next)

@main def main: Unit =
  println(
    takeFirst(MyList.of(1, 2, 3, 4, 5), 3)
  )

  println(
    takeWhile(MyList.of(1, 2, 3, 4, 5), (x: Int) => x != 4)
  )

  println(
    indexOf(MyList.of(1, 2, 3, 4, 5), 4)
  )

  println(
    indexOfNth(MyList.of(1, 2, 3, 4, 5, 4, 6, 4), 4, 2)
  )

  println(
    unfold(1) { i => if (i < 4) Some(i + 1) else None }
  )
