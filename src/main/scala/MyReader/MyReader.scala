package MyReader

// the Reader monad wraps a function, taking a `configuration type` C and yield some type A
case class MyReader[C, A] (run: C => A) {

  def flatMap[B](f: A => MyReader[C, B]): MyReader[C, B] = {
    val chainedReaderOp: C => B = (config: C) => f(this.run(config)).run(config)
    MyReader(run = chainedReaderOp)
  }

  def map[B](f: A => B): MyReader[C, B] = {
    val chainedOp: C => B = (config: C) => f(this.run(config))
    MyReader(run = chainedOp)
  }
}

object MyReader {
  // wrap a return type value by yield a Reader, with the run function taking a config and returning wrapped value
  def apply[C, A](value: A): MyReader[C, A] = new MyReader(run = (_: C) => value)
}

// main function for demonstration
object ReaderDriver extends App {

  case class Config(configInt: Int)

  def addAnInt(newInt: Int): MyReader[Config, Int] = {
    MyReader(run = (config: Config) => MyReader(newInt).run(config))
  }

  def multiplyByInt(newInt: Int): MyReader[Config, Int] = {
    MyReader(run = (config: Config) => config.configInt * newInt)
  }

  def writeMessage(newInt: Int): MyReader[Config, String] = {
    MyReader(s"This is a message with $newInt")
  }

  val startConfig: Config = Config(1)

  val twoOpsDependentOnSingleConfig = for {
    firstConfigOp <- addAnInt(3)
    secondConfigOp <- multiplyByInt(5)
    thirdConfigOp <- writeMessage(14)
  } yield (firstConfigOp, secondConfigOp, thirdConfigOp)

  val result: (Int, Int, String) = twoOpsDependentOnSingleConfig.run(startConfig)
  println(s"The result of a chains of ops, taking a config is - $result")

}