case class MyStateMonad[S, A](run: S => (S, A)) {

  //map as composition of lift and flatMap
  def map[B](g: A => B): MyStateMonad[S, B] = flatMap(a => MyStateMonad.lift(g(a)))

  //flatMap `chains run functions together`
  def flatMap[B](g: A => MyStateMonad[S, B]): MyStateMonad[S, B] = {

    val runFunctionChained = (s0: S) => {
      // Use the give state, s0, to execute the `run` function on THIS instance
      // Thus, you get a newState and a stateUpdate
      val (newState, stateUpdate) = this.run(s0)
      // Then, convert the state update into new instance of state monad (`g(a)`)
      // Finally, execute `run` on THAT instance, given the newState
      g(stateUpdate).run(newState)
    }
    // Return new instance of monad type with chained run function
    MyStateMonad(run = runFunctionChained)
  }
}

object MyStateMonad {
  def lift[S, A](stateUpdate: A): MyStateMonad[S, A] = MyStateMonad(run = s => (s, stateUpdate))
}

object DriveStateMonad extends App {

  case class GameState(state: Int)

  val beginningState = GameState(0)

  def updateGameState(stateUpdate: Int): MyStateMonad[GameState, Int] = MyStateMonad { s: GameState => {
    val newState = s.state + stateUpdate
    (GameState(state = newState), newState)
   }
  }

//  val updatedState = for {
//    _ <- updateGameState(4)
//    _ <- updateGameState(10)
//    lastState <- updateGameState(15)
//  } yield lastState
//

  // Let's expand the for comprehension to see what the compiler sees
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
