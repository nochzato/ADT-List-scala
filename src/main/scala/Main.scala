package MyList

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

  override def toString: String =
    @tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.append("]").result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

  def size: Int =
    @tailrec
    def go[A](xs: MyList[A], result: Int = 0): Int =
      xs match
        case MyNil          => result
        case MyCons(hd, tl) => go(tl, result + 1)
    go(this)

  def takeFirst(n: Int): MyList[A] =
    @tailrec
    def go[A](xs: MyList[A], n: Int, result: MyList[A] = MyNil): MyList[A] =
      xs match
        case MyNil => result.reverse
        case MyCons(hd, tl) =>
          if n > 0 then go(tl, n - 1, MyCons(hd, result)) else result.reverse
    go(this, n)

  def takeWhile(p: A => Boolean): MyList[A] =
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
    go(this, p)

  def indexOf[A](a: A): Option[Int] =
    @tailrec
    def go[A](xs: MyList[A], a: A, index: Int = 0): Option[Int] =
      xs match
        case MyNil => None
        case MyCons(hd, tl) =>
          if hd == a then Some(index)
          else go(tl, a, index + 1)
    go(this, a)

  // def indexOfNth[A](a: A, position: Int): Option[Int] =
  //   @tailrec
  //   def go[A](xs: MyList[A], a: A, position: Int, index: Int = 0): Option[Int] =
  //     position match
  //       case 1 => Some(index + this.indexOf(a).get)
  //       case _ => go(xs, a, position - 1, index + this.indexOf(a).get)

  //   go(this, a, position)

  def indexOfNth[A](a: A, position: Int): Option[Int] =
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
    go(this, a, position)

  def unfold[A](a: A)(next: A => Option[A]): MyList[A] =
    @tailrec
    def go[A](a: A, result: MyList[A])(next: A => Option[A]): MyList[A] =
      next(a) match
        case None    => result.reverse
        case Some(i) => go(i, MyCons(a, result))(next)
    go(a, MyNil)(next)

import MyList.*

object MyList {
  def apply[A](xs: A*) = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }
}

@main def main: Unit =
  println(
    MyList(1, 2, 3, 4, 5).takeFirst(3)
  )

  println(
    MyList(1, 2, 3, 4, 5).takeWhile((x: Int) => x != 4)
  )

  println(
    MyList(1, 2, 3, 4, 5).indexOf(4)
  )

  println(
    MyList(1, 2, 3, 4, 5, 4, 6, 4).indexOfNth(4, 2)
  )

  println(
    MyList().unfold(1) { i => if (i < 4) Some(i + 1) else None }
  )
