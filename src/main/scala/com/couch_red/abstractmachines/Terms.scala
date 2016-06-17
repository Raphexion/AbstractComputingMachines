package com.couch_red.abstractmachines

// page 105 - Abstract Computing Machines - Kluge

abstract class Expression

case class DeBruijn(n: Int) extends Expression
case class Application(e0: Expression, e1: Expression) extends Expression
case class Abstraction(exp: Expression) extends Expression
case class Suspension(env: List[Expression], exp: List[Expression]) extends Expression

case class Dummy(n: Int) extends Expression
