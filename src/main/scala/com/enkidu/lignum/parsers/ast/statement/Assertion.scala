package com.enkidu.lignum.parsers.ast.statement

import com.enkidu.lignum.parsers.ast.expression.Expression

case class Assertion(condition: Expression, detail: Option[Expression]) extends Statement {
  override def dispatch(visitor: Visitor): Unit = {
    condition.dispatch(visitor)
    detail.dispatch(visitor)
    apply(visitor)
  }
}
