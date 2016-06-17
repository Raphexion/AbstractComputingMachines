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

  test("Minimal possible program that checks the machine") {
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

      val ts3 = transfer(ts2)
      assert(ts3.environment == Nil)
      assert(ts3.terms == identity::Nil)
      assert(ts3.stack == Nil)
    }
  }
}
