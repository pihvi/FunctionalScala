package samples

import org.scalatest.BeforeAndAfterAll
import org.scalatest.junit.JUnitSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ScalaTestFunSuite extends FunSuite with Checkers {
  test("pop is invoked on an empty stack") {
    assert(3 === 2)
  }

  test("concat list") {
    check((a: List[Int], b: List[Int]) => a.size + b.size == (a ::: b).size)
  }
}