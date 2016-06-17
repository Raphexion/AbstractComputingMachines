package com.couch_red.abstractmachines

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class KMachineSuite extends FunSuite {
  import com.couch_red.abstractmachines.KMachine._

  trait TestVars {
    val deBruijn0 = DeBruijn(0)
    val identity = Abstraction(deBruijn0)
    val minimal = Application(identity, identity)
  }

  test("Program on page 106 - Abstract Computing Machines") {
    new TestVars {
      val ts0 = new TransitionState(Nil, minimal::Nil, Nil)

      val ts1 = transfer(ts0)
      assert(ts1.environment == Nil)
      assert(ts1.terms == identity::Nil)
      assert(ts1.stack == Suspension(Nil, identity::Nil) :: Nil)

      val ts2 = transfer(ts1)
      assert(ts2.environment == Suspension(Nil, identity::Nil)::Nil)
      assert(ts2.terms == deBruijn0::Nil)
      assert(ts2.stack == Nil)
    }
  }
}
