package com.enkidu.lignum.parsers.ast.statement.switch

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class SwitchStatement(expression: Expression, cases: Seq[SwitchRule]) extends Statement {
  override def dispatch(visitor: Visitor): Unit = {
    expression.dispatch(visitor)
    cases.dispatch(visitor)
    apply(visitor)
  }
}