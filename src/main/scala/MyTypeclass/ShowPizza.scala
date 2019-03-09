package MyTypeclass

// A typeclass in Scala consists of three pieces -
// 1) A trait, defining the methods of the typeclass
// 2) A "typeclass instance" object, with implicit classes implementing the specific behaviours of the typeclass
// 3) An "interface object", defining

// the "definition" of the typeclass - a contract, describing behaviours
trait PrettyPrinter[A] {
  def show(thingToPrint: A): String
}

// Typeclass instance - this is where we define the show method
object PrettyPrinterInstances {

  implicit val pizzaString: PrettyPrinter[Pizza] = new PrettyPrinter[Pizza] {
    def show(pizzaToPrint: Pizza): String = {
      val returnString: String =
        s"""This pizza has a ${pizzaToPrint.crustSize}, ${pizzaToPrint.crustType} crust. Also, it has the following toppings :\n"""
      pizzaToPrint.toppings.foldLeft(returnString)((currentList: String, topping: Topping) => currentList + topping + "\n")

    }
  }
}

// Syntax definition -  implicit classes can't be defined at top level - must be inside object, for example
object PizzaPrinterSyntax {
  implicit class PizzaPrinterOps[Pizza](pizzaToPrint: Pizza) {
    def asString(implicit prettyPrinterInstance: PrettyPrinter[Pizza]): String = {
      prettyPrinterInstance.show(pizzaToPrint)
    }
  }
}
