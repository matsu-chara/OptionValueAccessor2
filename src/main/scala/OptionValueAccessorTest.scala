import OptionValueAccessor._

object OptionValueAccessorTest {

  case class A(foo: Int)

  case class B(bar: Option[A])

  case class C(hoge: Option[B])

  case class D(fuga: Option[C])

  case class E(value: D)

  def main(args: Array[String]) {
    val edcba: Option[E] = Some(E(D(Some(C(Some(B(Some(A(1)))))))))

    val x: Option[Int] = OptValue(edcba).wrap.value.fuga.hoge.bar.foo
    println(x)
  }
}
