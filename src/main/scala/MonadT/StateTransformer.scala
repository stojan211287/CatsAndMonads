package MonadT

import scala.language.higherKinds

// The monad transformer wraps a `computation`, generating SOME MONAD, wrapping the (newState, newStateValue) tuple
case class StateTransformer[M[_], S, A](computation: S => M[(S, A)]) {

  def flatMap[B](f: A => StateTransformer[M, S, B])(implicit monad: MonadTrait[M]): StateTransformer[M, S, B] = {

    // here, the `monad.flatMap` method has TWO parameter groups -
    // 1st one is the monad wrapping (S, A) i.e. ANY MONAD M - IO, Option, Future, etc...
    // 2nd one a function S => M[(S, B)], wrapping a stateful computation S => (S, B) inside ANY MONAD M
    //                                                 1st parameter group
    //                                                 monad M[(S, A)] - result of this.computation(state)
    val newComputation: S => M[(S, B)] = (state: S) => monad.flatMap(this.computation(state)){
      // 2nd parameter group of MonadTrait[M] - a partial function, returning a monad M[(S, B)]
      // The reason it is a partial function is to make sure the value wrapped by M is a tuple (S, B)
      // i.e. the function to be flat-mapped across M is only defined for tuples of type (S, B)
      // the `f(newStateValue).computation(newState)` part is exactly the formula, defining a flatMap
      // for a State monad.
      // Thus, the StateTransformer encapsulates a state transition, wrapped in ANY MONAD M
      case (newState, newStateValue) => f(newStateValue).computation(newState)
    }
    // Return new StateTransformer with an updated computation function
    StateTransformer(computation = newComputation)
  }

  def map[B](f: A => B)(implicit monad: MonadTrait[M]): StateTransformer[M, S, B] = {
    val liftedFunction: A => StateTransformer[M, S, B] = (a: A) => StateTransformer.point(f(a))
    flatMap(f = liftedFunction)
  }
}

// Usual deal - makes it easy to write the `map` method
object StateTransformer {
  def point[M[_], S, A](value: A)(implicit monad: MonadTrait[M]): StateTransformer[M, S, A] = {
    val computationFunction: S => M[(S, A)] = (s: S) => monad.lift((s, value))
    StateTransformer(computation = computationFunction)
  }
}