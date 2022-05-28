import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import MyList._
import MyList.*

class MyListSuite extends ScalaCheckSuite {

  given [A: Arbitrary]: Arbitrary[MyList[A]] =
    Arbitrary(Gen.listOf(arbitrary[A]).map(MyList.of(_*)))

  property(
    "takeFirst should return a list that is smaller than argument or equals it"
  ) {
    forAll { (xs: MyList[Int], n: Int) =>
      xs.takeFirst(n).size <= xs.size
    }
  }

  property("takeFirst should return argument's sublist") {
    forAll { (xs: MyList[Int], n: Int) =>
      xs.takeFirst(n).isSubList(xs)
    }
  }

  property(
    "takeFirst should return identical list if n is bigger than argument's size or equals it"
  ) {
    forAll { (xs: MyList[Int], n: Int) =>
      if n >= xs.size then xs.takeFirst(n) == xs
    }
  }

  property(
    "takeWhile should return a list that is smaller than argument or equals it"
  ) {
    forAll { (xs: MyList[Int], p: Int => Boolean) =>
      xs.takeWhile(p).size <= xs.size
    }
  }

  property("takeWhile should return argument's sublist") {
    forAll { (xs: MyList[Int], p: Int => Boolean) =>
      xs.takeWhile(p).isSubList(xs)
    }
  }

  property(
    "takeWhile should return identical list if condition is always true"
  ) {
    forAll { (xs: MyList[Int]) =>
      xs.takeWhile(_ => true) == xs
    }
  }

  property("indexOf should return a number that is smaller than list's size") {
    forAll { (xs: MyList[Int], a: Int) =>
      xs.indexOf(a) match
        case None    => true
        case Some(i) => i <= xs.size - 1
    }
  }

  property("indexOf should return None if argument is empty list") {
    forAll { (a: Int) =>
      MyNil.indexOf(a) == None
    }
  }

  property(
    "indexOfNth should return a number that is smaller than list's size"
  ) {
    forAll { (xs: MyList[Int], a: Int, position: Int) =>
      xs.indexOfNth(a, position) match
        case None    => true
        case Some(i) => i <= xs.size - 1
    }
  }

  property("indexOfNth should return None if argument is empty list") {
    forAll { (a: Int, position: Int) =>
      MyNil.indexOfNth(a, position: Int) == None
    }
  }

  property(
    "indexOfNth should return same number as indexOf if position equals 1"
  ) {
    forAll { (xs: MyList[Int], a: Int) =>
      xs.indexOfNth(a, 1) == xs.indexOf(a)
    }
  }

  property("unfold should return empty list if condition is always None") {
    forAll { (xs: MyList[Int], a: Int) =>
      xs.unfold(a)(_ => None) == MyNil
    }
  }

}
