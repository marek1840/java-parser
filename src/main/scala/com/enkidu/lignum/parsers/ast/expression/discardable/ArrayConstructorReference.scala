package com.enkidu.lignum.parsers.ast.expression.discardable

import com.enkidu.lignum.parsers.ast.expression.types.references.ArrayType

case class ArrayConstructorReference(`type`: ArrayType) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    apply(visitor)
  }
}

