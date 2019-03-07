package MonadT

object TransformerDemo extends App {

    def toInt(someString: String): Int = {
      try {
        someString.toInt
      } catch {
        case _: Exception => 0
      }
    }

    // A state class - `type S`
    case class SumState(sum: Int)

    // an implementation of the `Monad` trait for the `IO` type.
    implicit val IOMonad: MonadTrait[IO] = new MonadTrait[IO] {
      def lift[A](a: => A): IO[A] = IO(a) //  use `apply`, defined in the IO monad as a `lift`
      def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
    }

    def doSumWithStateTransformer(newValue: Int): StateTransformer[IO, SumState, Int] = {
      val newStateTTransition = (oldState: SumState) => {
        val newSum = newValue + oldState.sum
        val newState: SumState = oldState.copy(sum = newSum)
        // Return the state tuple (S, B), wrapped in the IO monad
        IO(newState, newSum)
      }
      StateTransformer(computation = newStateTTransition)
    }

    def liftIoIntoStateTransformer[A](io: IO[A]): StateTransformer[IO, SumState, A] = StateTransformer { s =>
      io.map(a => (s, a))  // yields the type `[IO(SumState, A)]`
    }

    def getLineAsStateTransformer: StateTransformer[IO, SumState, String] = {
      val sideEffectFunction = () => scala.io.StdIn.readLine() // a function with side-effects takes no params - no RT
      val monadWrapOfSideEffect: IO[String] = IO(sideEffectFunction())
      liftIoIntoStateTransformer(monadWrapOfSideEffect)
    }

    def putStrAsStateT(stringToShow: String): StateTransformer[IO, SumState, Unit]   = {
      val showUserInput: String =>  Unit = (printThis: String) => println(printThis)
      val printWrappedByIO: IO[Unit] = IO(showUserInput(stringToShow))
      liftIoIntoStateTransformer(printWrappedByIO)
    }

    def sumLoop: StateTransformer[IO, SumState, Unit] = for {
      _     <- putStrAsStateT("\nPlease type in an integer, or 'q' to quit: ")
      input <- getLineAsStateTransformer
      _     <- if (input == "q") {
        liftIoIntoStateTransformer(IO(Unit))
      } else for {
        i <- liftIoIntoStateTransformer(IO(toInt(input)))
        _ <- doSumWithStateTransformer(i)
        _ <- sumLoop
      } yield Unit
    } yield Unit

    val result = sumLoop.computation(SumState(0)).code
    println(s"Final SumState: $result")

}