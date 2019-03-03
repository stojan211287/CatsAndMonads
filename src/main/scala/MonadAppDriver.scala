object MonadAppDriver {

  // TODO : This requires wrappers extending a Monad trait of some kind
  def testMonad[M](monad: M): Unit = ???

  def main(args: Array[String]): Unit = {

    println("TESTING CUSTOM WRAPPER\n")
    // Test my Wrapper monad with companion object
    val objectForComp = for {
      a <- WrapperWithObject(3)
      b <- WrapperWithObject(24)
      c <- WrapperWithObject(234)
    } yield a*b*c
    println(s"Custom wrappers work in for comprehension. Result :  $objectForComp.\n")

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

    println("TESTING CUSTOM READER MONAD\n")
    // Test CorrectOrComplaint - a version of the Reader monad
    def doAThingWhileTalking(thingDoer: Int =>  Int, whatThing: String)(num: Int): CorrectOrComplaint[Int] = {
      val newNum: Int = thingDoer(num)
      val newMessage: String = s"$whatThing"
      CorrectOrComplaint(possiblyCorrectValue = newNum, complaintMessage = newMessage)
    }

    val readerComplains = for {
      first <- doAThingWhileTalking(_*3, "Multiplied by 3")(num = 2)
      second <- doAThingWhileTalking(_ + 2, "Added 2")(num = first)
      third <- doAThingWhileTalking(_ / 4, "Divided by 4")(num = second)
    } yield third
    println(s"The result of testing my custom Reader monad is ${readerComplains.possiblyCorrectValue}.")
    println("It was obtained through these steps - \n")
    println(s"${readerComplains.complaintMessage}")

  }
}
