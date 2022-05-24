import scala.annotation.tailrec
enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

import MyList.*

object MyList {
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }
}

// TODO rewrite to tailrec
def takeFirst[A](xs: MyList[A], n: Int): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      if n > 0 then MyCons(hd, takeFirst(tl, n - 1)) else MyNil

def takeWhile[A](xs: MyList[A], p: A => Boolean): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      if (p(hd)) then MyCons(hd, takeWhile(tl, p)) else MyNil

def unfold[A](a: A)(next: A => Option[A]): MyList[A] =
  next(a) match
    case None    => MyNil
    case Some(i) => MyCons(a, unfold(i)(next))

def indexOf[A](xs: MyList[A], a: A): Option[Int] =
  @tailrec
  def go[A](xs: MyList[A], a: A, index: Int): Option[Int] =
    xs match
      case MyNil => None
      case MyCons(hd, tl) =>
        if hd == a then Some(index)
        else go(tl, a, index + 1)
  go(xs, a, 0)

def indexOfNth[A](xs: MyList[A], a: A, position: Int): Option[Int] =
  def go[A](xs: MyList[A], a: A, position: Int, index: Int): Option[Int] =
    xs match
      case MyNil => None
      case MyCons(hd, tl) =>
        if hd == a then
          position match
            case 1 => Some(index)
            case _ => go(tl, a, position - 1, index + 1)
        else go(tl, a, position, index + 1)
  go(xs, a, position, 0)

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
