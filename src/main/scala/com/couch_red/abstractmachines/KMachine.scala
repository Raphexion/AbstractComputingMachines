package com.couch_red.abstractmachines

case class MachineFinished(state: TransitionState) extends Exception

object KMachine {
  def transfer(state: TransitionState): TransitionState = {
    val E = state.environment
    val T = state.terms
    val S = state.stack

    T match {
      case DeBruijn(0) :: _ts => {
        E match {
          case Suspension(env, exp) :: _es => new TransitionState(env, exp, S)
        }
      }

      case DeBruijn(n) :: ts => {
        state.environment match {
          case _ :: es => new TransitionState(es, DeBruijn(n - 1) :: ts, state.stack)
        }
      }

      case Application(e0, e1) :: _ts => {
        new TransitionState(E, e0 :: Nil, Suspension(E, e1 :: Nil) :: S)
      }

      case Abstraction(e0) :: _ts => {
        state.stack match {
          case s :: tail => new TransitionState(s :: E, e0 :: Nil, tail)
          case _ => throw new MachineFinished(state)
        }
      }
    }
  }

  def run_machine(state: TransitionState): TransitionState = {
    if (state.terms == Nil) state
    else run_machine(transfer(state))
  }
}
