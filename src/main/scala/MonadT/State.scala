package MonadT

case class State[S, A](transitionState: S => (S, A)){

  def flatMap[B](statefulComputation: A => State[S, B]): State[S, B] = {

    val chainedStatefulComputation: S => (S, B) = (givenState: S) => {
      // make first state transition
      val (stateAfterFirstChange, changedStateAttribute ) : (S, A) = this.transitionState(givenState)
      // compute new state, given statefulComputation
      val newChangeOfState: State[S, B] = statefulComputation(changedStateAttribute)
      // make a `second` change of state, using newChangeOfState, thereby chaining two stateful computations
      newChangeOfState.transitionState(stateAfterFirstChange)
    }
    // This is what you return - a new monad instance, wrapping two chained stateful computations
    State(transitionState = chainedStatefulComputation)
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State(f(a)))
}

// `apply` method so we can easily define map in the above case class
object State {
  def apply[S, A](stateChange: A): State[S, A] = State(transitionState = (s: S) => (s, stateChange))
}
