package CatsExercises

import cats.Show

// implement Int and String Cats instances, that have implicit implementations of the Show[A] trait
import cats.instances.int._
import cats.instances.string._

// import the Cats Show syntax
import cats.syntax.show._

// To define the Show class for Date
import java.util.Date

object ShowTypeclass extends App {

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("IAmAString")

  val shownInt: String = 123.show
  val shownString: String = "IAmAString".show

  println(intAsString)
  println(stringAsString)

  println(shownInt)
  println(shownString)

  // Define a Show instance for a custom class
//  implicit val dateShow: Show[Date] = new Show[Date] {
//    def show(date: Date): String = s"${date.getTime/1000}ms since the epoch."
//  }
  // .. or, you can use the utility function
  implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime/1000} ms since the epoch.")

  val newDate: Date = new Date(2011, 11, 1)
  val dateToShow: String = newDate.show

  println(dateToShow)
}