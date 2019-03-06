package MonadT

class IO[A] private (someCode: => A) {

  // Need to do this to access a private attribute
  def code: A = this.someCode

  def flatMap[B](safeComputation: A => IO[B]): IO[B] = IO(safeComputation(code).code)
  def map[B](unsafeComputation: A => B): IO[B] = flatMap(comp => IO(unsafeComputation(comp)))
}

object IO {
  def apply[A](unsafeComputation: A) = new IO(someCode = unsafeComputation)
}


