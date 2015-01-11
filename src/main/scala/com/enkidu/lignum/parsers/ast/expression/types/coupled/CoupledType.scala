package com.enkidu.lignum.parsers.ast.expression.types.coupled

import com.enkidu.lignum.parsers.ast.expression.types.Type

trait CoupledType extends Type {
  def types: Seq[Type]

  override def dispatch(visitor: Visitor): Unit = {
    types.dispatch(visitor)
    apply(visitor)
  }
}
