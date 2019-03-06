package MonadT

// We need this for the following reason :
// A Monad Transformer is a MONAD itself, BUT in can ONLY wrap types that are themselves MONADS.
// Thus, we need a trait, defining what it means to be a monad, in order to write monad transformer code
// that makes sure that it's being applied to a monad.

// Scala has support for higher-kinded types. Yay! But, we have to import it
import scala.language.higherKinds

trait MonadTrait[M[_]] {
  // also called `pure`, or `point` or `return` - straightforward
  def lift[A](a: => A): M[A]
  // flatMap has two parameter groups -  the first one representing the monad instance the flatMap will be a method of
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  // same deal with `map` as with `flatMap`. Also, we can define `map` in terms of `flatMap` right in the trait
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma: M[A])(a => lift[B](f(a)))
}
