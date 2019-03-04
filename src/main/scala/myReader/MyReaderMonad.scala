package myReader

// Here, I've decided to implement the CorrectOrComplaint monad using the `unit (lift) - flatMap` pattern
// Then, map := flatMap(lift . f) or rather flatmap(f andThen lift)
case class MyReaderMonad[A](computeValue: A, message: String) {

  def flatMap[B](f: A => MyReaderMonad[B]): MyReaderMonad[B] = {
    val possiblyComplaint = f(this.computeValue)
    MyReaderMonad(
      computeValue = possiblyComplaint.computeValue,
      message = this.message+"\n"+possiblyComplaint.message
    )
  }

  def map[B](f: A => B): MyReaderMonad[B] = {
    val liftedFunction: A => MyReaderMonad[B] = f andThen MyReaderMonad.lift
    val returnMonadElement: MyReaderMonad[B] = flatMap(liftedFunction)
    returnMonadElement
  }

}
// Put `lift` method into companion object
object  MyReaderMonad{
  def lift[A](unliftedValue: A): MyReaderMonad[A] = MyReaderMonad(
    computeValue = unliftedValue,
    message="")
}

object ReaderDriver extends App {

  println("TESTING CUSTOM READER MONAD\n")
  // Test CorrectOrComplaint - a version of the Reader monad
  def doAThingWhileTalking(thingDoer: Int =>  Int, thingItDid: String)(num: Int): MyReaderMonad[Int] = {
    val newNum: Int = thingDoer(num)
    val newMessage: String = s"$thingItDid"
    MyReaderMonad(computeValue = newNum, message = newMessage)
  }

  val readerComplains = for {
    first <- doAThingWhileTalking(_*3, "Multiplied by 3")(num = 2)
    second <- doAThingWhileTalking(_ + 2, "Added 2")(num = first)
    third <- doAThingWhileTalking(_ / 4, "Divided by 4")(num = second)
  } yield third
  println(s"The result of testing my custom Reader monad is ${readerComplains.computeValue}.")
  println("It was obtained through these steps - \n")
  println(s"${readerComplains.message}")

}
