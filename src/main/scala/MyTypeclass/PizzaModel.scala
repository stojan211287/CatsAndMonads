package MyTypeclass

sealed trait Topping
case object Cheese extends Topping
case object Pepperoni extends Topping
case object Mushrooms extends Topping
case object Salami extends Topping
case object Mozzarella extends Topping
case object Pecorino extends Topping


sealed trait CrustSize
case object SmallCrust extends CrustSize
case object MediumCrust extends CrustSize
case object LargeCrust extends CrustSize

sealed trait CrustType
case object ThinCrust extends CrustType
case object ThickCurst extends CrustType
case object RegularCrust extends CrustType

case class Pizza(
                crustType: CrustType,
                crustSize: CrustSize,
                toppings: List[Topping]
                )