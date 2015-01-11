package com.enkidu.lignum.parsers.ast.statement.switch

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class SwitchCases(cases: Seq[Expression], statements: Seq[Statement]) extends SwitchRule {
  override def dispatch(visitor: Visitor): Unit = {
    cases.dispatch(visitor)
    statements.dispatch(visitor)
    apply(visitor)
  }
}