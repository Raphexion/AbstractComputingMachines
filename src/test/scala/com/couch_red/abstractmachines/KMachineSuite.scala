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

    // (A.#0 A.#0)
    val minimal = Application(identity, identity)

    // ((A.A.(A.#0 #0) A.#0) A.#0)
    val ab_minimal = Abstraction(minimal)
    val ab_ab_minimal = Abstraction(ab_minimal)
    val ap_ab_ab_minimal = Application(ab_ab_minimal, identity)
    val program2 = Application(ap_ab_ab_minimal, identity)
  }

  test("A.#0") {
    new TestVars {
      val ts0 = new TransitionState(Nil, identity::Nil, Dummy(42)::Nil)

      val ts1 = transfer(ts0)
      assert(ts1.environment == Dummy(42)::Nil)
      assert(ts1.terms == deBruijn0::Nil)
      assert(ts1.stack == Nil)
    }
  }

  test("(A.#0 A.#0)") {
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

  test("((A.A.(A.#0 #0) A.#0) A.#0)") {
    new TestVars {
      val ts1 = transfer(new TransitionState(Nil, ab_minimal::Nil, Dummy(42)::Nil))
      assert(ts1.environment == Dummy(42)::Nil)
      assert(ts1.terms == minimal::Nil)
      assert(ts1.stack == Nil)
    }

    new TestVars {
      val ts1 = transfer(new TransitionState(Nil, ab_ab_minimal::Nil, Dummy(42)::Nil))
      assert(ts1.environment == Dummy(42)::Nil)
      assert(ts1.terms == ab_minimal::Nil)
      assert(ts1.stack == Nil)
    }

    new TestVars {
      val ts1 = transfer(new TransitionState(Nil, ap_ab_ab_minimal::Nil, Nil))
      assert(ts1.environment == Nil)
      assert(ts1.terms == ab_ab_minimal::Nil)
      assert(ts1.stack == Suspension(Nil, identity::Nil)::Nil)
    }

    new TestVars {
      val ts1 = transfer(new TransitionState(Nil, program2::Nil, Nil))
      assert(ts1.environment == Nil)
      assert(ts1.terms == ap_ab_ab_minimal::Nil)
      assert(ts1.stack == Suspension(Nil, identity::Nil)::Nil)
    }

    new TestVars {
      val res: TransitionState = try {
        run_machine(new TransitionState(Nil, program2::Nil, Nil))
      }
      catch {
        case MachineFinished(state) => state
      }
      assert(res.terms == identity::Nil)
      assert(res.stack == Nil)
    }
  }
}
