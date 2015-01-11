package com.enkidu.lignum.parsers.ast.statement.loop

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class DoWhile(body: Statement, condition: Expression) extends Loop {
  override def dispatch(visitor: Visitor): Unit = {
    body.dispatch(visitor)
    condition.dispatch(visitor)
    apply(visitor)
  }
}