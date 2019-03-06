package MyOption

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
}

// It is IMPORTANT that this is a case OBJECT - THERE IS ONLY ONE `Zilch`
case object Zilch extends MyOption[Nothing] {
  override def map[B](f: Nothing => B): MyOption[B] = Zilch
  override def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = Zilch
}

// This is a case class, because many instances of it may be produced, containing different values
case class Truly[A](value: A) extends MyOption[A] {

  override def map[B](f: A => B): MyOption[B] = {
    val newTrueValue = f(this.value)
    Truly(value = newTrueValue)
  }
  override def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    f(this.value)
  }
}

object OptionDriver extends App {

  def makeAnIntegerThing(thing: String): MyOption[Int] = {
    try {
      val intThing = thing.toInt
      Truly(value = intThing)
    }
    catch {
      case _: Exception => Zilch
    }
  }

  println("TESTING CUSTOM OPTION MONAD\n")
  // Test MyOption
  val myOptionForComp = for {
    a <- makeAnIntegerThing(thing="3")
    b <- makeAnIntegerThing(thing="noThingWhatsoever")
  } yield a*b
  println(s"The result is $myOptionForComp. The result SHOULD BE Zilch.\n")
}
