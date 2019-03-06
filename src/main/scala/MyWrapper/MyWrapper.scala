package MyWrapper

case class MyWrapper[A] private(wrappedValue: A) {

  def map(f: A => A): MyWrapper[A] = {
    val newWrappedValue = f(this.wrappedValue)
    new MyWrapper(wrappedValue = newWrappedValue)
  }
  def flatMap(f: A => MyWrapper[A]): MyWrapper[A] = {
    val newWrapper: MyWrapper[A] = f(this.wrappedValue)
    newWrapper
  }
  // For printing purposes
  override def toString: String = wrappedValue.toString
}

object MyWrapper {
  def apply[A](value: A): MyWrapper[A] = new MyWrapper(wrappedValue = value)
}

object WrapperDriver extends App {

  println("TESTING CUSTOM WRAPPER\n")
  // Test my Wrapper monad with companion object
  val objectForComp = for {
    a <- MyWrapper(3)
    b <- MyWrapper(24)
    c <- MyWrapper(234)
  } yield a*b*c
  println(s"Custom wrappers work in for comprehension. Result :  $objectForComp.\n")

}