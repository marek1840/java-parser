package com.enkidu.lignum.parsers.ast.statement.flow

import com.enkidu.lignum.parsers.ast.expression.Expression

case class Throw(exception: Expression) extends FlowStatement {
  override def dispatch(visitor: Visitor): Unit = {
    exception.dispatch(visitor)
    apply(visitor)
  }
}