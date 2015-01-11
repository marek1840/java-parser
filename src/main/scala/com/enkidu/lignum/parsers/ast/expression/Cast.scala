package com.enkidu.lignum.parsers.ast.expression

import com.enkidu.lignum.parsers.ast.expression.types.Type

case class Cast(target: Type, source: Expression) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    target.dispatch(visitor)
    source.dispatch(visitor)
    apply(visitor)
  }
}
