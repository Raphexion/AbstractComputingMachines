package com.couch_red.abstractmachines

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class KMachineSuite extends FunSuite {
  import com.couch_red.abstractmachines.KMachine._

  trait TestVars {
    val identity = Abstraction(DeBruijn(0))
    val minimal = Application(identity, identity)
  }

  test("Program on page 106 - Abstract Computing Machines") {
    new TestVars {
      val ts0 = new TransitionState(Nil, minimal::Nil, Nil)
      val ts1 = transfer(ts0)
      assert(ts1.environment == Nil)
      assert(ts1.stack == Suspension(Nil, identity::Nil) :: Nil)
    }
  }
}
