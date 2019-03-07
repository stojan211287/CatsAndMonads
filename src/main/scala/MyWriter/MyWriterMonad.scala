package MyWriter

// Here, I've decided to implement the CorrectOrComplaint monad using the `unit (lift) - flatMap` pattern
// Then, map := flatMap(lift . f) or rather flatmap(f andThen lift)
case class MyWriterMonad[A](computeValue: A, message: String) {

  def flatMap[B](f: A => MyWriterMonad[B]): MyWriterMonad[B] = {
    val possiblyComplaint = f(this.computeValue)
    MyWriterMonad(
      computeValue = possiblyComplaint.computeValue,
      message = this.message+"\n"+possiblyComplaint.message
    )
  }

  def map[B](f: A => B): MyWriterMonad[B] = {
    val liftedFunction: A => MyWriterMonad[B] = f andThen MyWriterMonad.lift
    val returnMonadElement: MyWriterMonad[B] = flatMap(liftedFunction)
    returnMonadElement
  }

}
// Put `lift` method into companion object
object  MyWriterMonad{
  def lift[A](unliftedValue: A): MyWriterMonad[A] = MyWriterMonad(
    computeValue = unliftedValue,
    message="")
}

object WriterDriver extends App {

  println("TESTING CUSTOM READER MONAD\n")
  // Test CorrectOrComplaint - a version of the Reader monad
  def doAThingWhileTalking(thingDoer: Int =>  Int, thingItDid: String)(num: Int): MyWriterMonad[Int] = {
    val newNum: Int = thingDoer(num)
    val newMessage: String = s"$thingItDid"
    MyWriterMonad(computeValue = newNum, message = newMessage)
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
