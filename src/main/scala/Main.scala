enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

import MyList.*

object MyList {
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }
}

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

// not working yet
def indexOf[A](xs: MyList[A], a: A): Option[Int] =
  xs match
    case MyNil          => None
    case MyCons(hd, tl) => if hd == a then Some(???) else indexOf(tl, a)

def indexOfNth[A](xs: MyList[A], a: A, index: Int): Option[Int] =
  xs match
    case MyNil          => None
    case MyCons(hd, tl) => ???

def unfold[A](a: A)(next: A => Option[A]): List[A] = ???

@main def main: Unit =
  println(
    takeWhile(MyList.of(1, 2, 3, 4, 5), (x: Int) => x != 4)
  )
  println(
    indexOf(MyList.of(1, 2, 3, 4, 5), 8)
  )
