package myState

case class StateTransition[S, A](run: S => (S, A)) {

  //map as composition of lift and flatMap
  def map[B](f: A => B): StateTransition[S, B] = flatMap(f andThen StateTransition.lift)

  //flatMap `chains run functions together`
  def flatMap[B](f: A => StateTransition[S, B]): StateTransition[S, B] = {

    val chainedRunFunction = (initialState: S) => {

      // `run` first state transition, starting from initialState
      val (stateAfterFirstTransition, stateChangeOfTypeA) = this.run(initialState)

      // generate secondStateTransition - from stateChangeOfTypeA
      val secondStateTransition = f(stateChangeOfTypeA)
      //
      val (stateAfterSecondTransition, stateChangeOfTypeB) = secondStateTransition.run(stateAfterFirstTransition)

      (stateAfterSecondTransition, stateChangeOfTypeB)
    }
    // Return new instance of monad type with chained run function
    StateTransition(run = chainedRunFunction)
  }
}

object StateTransition {
  def lift[S, A](stateUpdate: A): StateTransition[S, A] = StateTransition(run = s => (s, stateUpdate))
}

object StateDriver extends App {

  case class GameState(state: Int)

  val beginningState = GameState(0)

  // This function is needed to define the `first` run function and kickoff the State monad chaining
  // Also, unsurprisingly, it tells the chain of State monads `how` to update state at each step
  def updateGameState(stateUpdate: Int): StateTransition[GameState, Int] = StateTransition { s: GameState => {
    // This is the sum total of all state updates we do. Could be more complicated.
    val newState = s.state + stateUpdate
    (GameState(state = newState), newState)
   }
  }

  // This is how you would normally do monad chaining, but...
//  val updatedState = for {
//    _ <- updateGameState(4)
//    _ <- updateGameState(10)
//    lastState <- updateGameState(15)
//  } yield lastState
//

  // ...let's expand the for comprehension to see what the compiler sees
  val updatedState =
    updateGameState(4).flatMap(_ =>
      updateGameState(10).flatMap( _ =>
        updateGameState(15).map(lastState => lastState)
      )
    )

  val stateUpdatedOnce = updateGameState(15).map(lastState => lastState)
  // stateUpdatedOnce has run: s => {(GameState(s.state+15, s.state+15)}
  println("State after bottom update: ", stateUpdatedOnce.run(beginningState))


  val stateUpdatedTwice = updateGameState(10).flatMap(_ => stateUpdatedOnce)
  /*
    stateUpdatedTwice has run: s => {
    val (GameState(s.state+10), 10) = this.run(s)
    ( _ => stateUpdatedOnce)(10).run(GameState(s.state+10))

    -  which reduces to -

    s => stateUpdatedOnce.run(GameState(s.state+10))
   */
  println("State after middle update: ", stateUpdatedTwice.run(beginningState))


  val stateUpdatedThrice = updateGameState(4).flatMap(_ => stateUpdatedTwice)
  /*
    stateUpdatedThrice has run: s => {
    val (GameState(s.state+4), 4) = this.run(s)
    (_ => stateUpdatedTwice)(4).run(GameState(s.state+4))

    - which reduces to -

    s => stateUpdatedTwice.run(GameState(s.state+4))
    }
  */
  println("State after top update: ", stateUpdatedThrice.run(beginningState))

  /*
    Let's put all this together and `unwind` the discussion above.

    You call `stateUpdatedThrice.run(beginningState)` - this is the same as `endState.run(beginningState)`

    This expands to -

    1) stateUpdatedThrice.run(beginningState)
    2)    s => stateUpdatedTwice.run(GameState(beginningState.state + 4))
    3)      s => stateUpdatedOnce.run(GameState(beginningState.state + 4 + 10))
    4)        s => (GameState(beginningState.state + 4 + 10 + 15, beginningState.state + 4 + 10 + 15)

    All your state updates have been chained together.

    Ta. Da.

   */
}
