// Here, I've decided to implement the CorrectOrComplaint monad using the `unit (lift) - flatMap` pattern
// Then, map := flatMap(lift . f) or rather flatmap(f andThen lift)
case class CorrectOrComplaint[A](possiblyCorrectValue: A, complaintMessage: String) {

  def lift[A](unliftedValue: A): CorrectOrComplaint[A] =
    CorrectOrComplaint(
      possiblyCorrectValue = unliftedValue,
      complaintMessage=""
    )

  def flatMap[B](f: A => CorrectOrComplaint[B]): CorrectOrComplaint[B] = {
    val possiblyComplaint = f(this.possiblyCorrectValue)
    CorrectOrComplaint(
      possiblyCorrectValue = possiblyComplaint.possiblyCorrectValue,
      complaintMessage = this.complaintMessage+"\n"+possiblyComplaint.complaintMessage
    )
  }

  def map[B](f: A => B): CorrectOrComplaint[B] = {
    val liftedFunction: A => CorrectOrComplaint[B] = f andThen lift
    val returnMonadElement: CorrectOrComplaint[B] = flatMap(liftedFunction)
    returnMonadElement
  }

}
