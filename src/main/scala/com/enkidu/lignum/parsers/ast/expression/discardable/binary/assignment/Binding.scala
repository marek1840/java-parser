package com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment

import com.enkidu.lignum.parsers.ast.expression.Expression

case class Binding(target: Expression, source: Expression) extends Assignment {
  override def dispatch(visitor: Visitor): Unit = {
    target.dispatch(visitor)
    source.dispatch(visitor)
    apply(visitor)
  }
}
