case class WrapperWithObject[A] private (wrappedValue: A) {

  def map(f: A => A): WrapperWithObject[A] = {
    val newWrappedValue = f(this.wrappedValue)
    new WrapperWithObject(wrappedValue = newWrappedValue)
  }
  def flatMap(f: A => WrapperWithObject[A]): WrapperWithObject[A] = {
    val newWrapper: WrapperWithObject[A] = f(this.wrappedValue)
    newWrapper
  }
  // For printing purposes
  override def toString: String = wrappedValue.toString
}

object WrapperWithObject {
  def apply[A](value: A): WrapperWithObject[A] = new WrapperWithObject(wrappedValue = value)
}