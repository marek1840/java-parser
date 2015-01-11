package com.enkidu.lignum.parsers.ast.statement.conditional

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class If(condition: Expression, ifTrue: Statement) extends ConditionalStatement {
  override def dispatch(visitor: Visitor): Unit = {
    condition.dispatch(visitor)
    ifTrue.dispatch(visitor)
    apply(visitor)
  }
}