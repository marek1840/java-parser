package com.enkidu.lignum.parsers.ast.expression.discardable.literals

import com.enkidu.lignum.parsers.ast.expression.types.Type

case class ClassLiteral(`type`: Type) extends Literal {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    apply(visitor)
  }
}
