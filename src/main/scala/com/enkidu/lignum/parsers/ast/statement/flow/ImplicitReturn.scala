package com.enkidu.lignum.parsers.ast.statement.flow

import com.enkidu.lignum.parsers.ast.expression.Expression

case class ImplicitReturn(value: Expression) extends FlowStatement {
  override def dispatch(visitor: Visitor): Unit = {
    value.dispatch(visitor)
    apply(visitor)
  }
}