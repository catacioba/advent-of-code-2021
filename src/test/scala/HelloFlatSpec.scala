import org.scalatest.flatspec.AnyFlatSpec

class HelloFlatSpec extends AnyFlatSpec {

  behavior of "Hello"

  it should "start with H" in {
    assert("Hello".startsWith("H"))
  }

}
