package com.enkidu.lignum.parsers.ast.statement.switch

import com.enkidu.lignum.parsers.ast.expression.Expression

case class EmptySwitchCases(cases: Seq[Expression]) extends SwitchRule {
  override def dispatch(visitor: Visitor): Unit = {
    cases.dispatch(visitor)
    apply(visitor)
  }
}