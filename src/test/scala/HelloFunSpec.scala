import org.scalatest.funsuite.AnyFunSuite

class HelloFunSpec extends AnyFunSuite {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }
}
