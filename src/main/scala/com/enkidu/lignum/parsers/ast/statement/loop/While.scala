package com.enkidu.lignum.parsers.ast.statement.loop

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class While(condition: Expression, body: Statement) extends Loop {
  override def dispatch(visitor: Visitor): Unit = {
    condition.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}