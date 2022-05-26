import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import MyList._
import MyList.*

class MyListSuite extends ScalaCheckSuite {

  given [A: Arbitrary]: Arbitrary[MyList[A]] =
    Arbitrary(Gen.listOf(arbitrary[A]).map(MyList.of(_*)))

  property("takeFirst shouldn't return list that is bigger than argument") {
    forAll { (xs: MyList[Int], n: Int) =>
      xs.takeFirst(n).size <= xs.size
    }
  }

}
