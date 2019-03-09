package MyTypeclass

// needed for importing the definition of the "show" method
import MyTypeclass.PrettyPrinterInstances.pizzaString
// needed for using the "*.asString" syntax
import MyTypeclass.PizzaPrinterSyntax.PizzaPrinterOps

object PizzaDriver extends App {

  val pizzaToppings: List[Topping] = List(Pecorino, Pepperoni, Mozzarella)
  val myPizza = Pizza(crustSize = LargeCrust, crustType = ThinCrust, toppings = pizzaToppings)

  //

  println(myPizza.asString)

}
