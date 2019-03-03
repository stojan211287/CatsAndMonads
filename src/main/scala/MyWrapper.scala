// IMPORTANT: val wrappedValue generates a getter by scalac
//            var wrappedValue generates both a getter and a setter
//            wrappedValue generates neither -  access is very restricted
class MyWrapper[A](val wrappedValue: A) {

  // IMPLEMENT YOUR OWN MAPPER FOR YOUR WRAPPER
  def map[B](f: A => B):MyWrapper[B] = {
    val newWrappedValue = f(this.wrappedValue)
    new MyWrapper(wrappedValue = newWrappedValue)
  }
  def flatMap[B](f: A => MyWrapper[B]): MyWrapper[B] = {
    val newWrapper = f(this.wrappedValue)
    newWrapper
  }
  override def toString: String = this.wrappedValue.toString
}
