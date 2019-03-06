package MonadT

import scala.language.higherKinds

case class StateTransformer[M[_], S, A](computation: S => M[(S, A)]) {

  def flatMap[B](f: A => StateTransformer[M, S, B])(implicit monad: MonadTrait[M]): StateTransformer[M, S, B] = {

    // here, the `monad.flatMap` method has TWO parameter groups -
    // 1st one is the monad wrapping (S, A) i.e. the state monad
    // 2nd one is the other computation that is being chained
    val newComputation: S => M[(S, B)] = (state: S) => monad.flatMap(computation(state)){
      // In case it is a State monad, it is possible to chain a computation
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