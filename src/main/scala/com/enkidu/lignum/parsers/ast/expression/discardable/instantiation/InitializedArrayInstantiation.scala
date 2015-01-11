package com.enkidu.lignum.parsers.ast.expression.discardable.instantiation

import com.enkidu.lignum.parsers.ast.expression.ArrayInitializer
import com.enkidu.lignum.parsers.ast.expression.types.Type

case class InitializedArrayInstantiation(`type`: Type, initializer: ArrayInitializer) extends ArrayInstantiation {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    initializer.dispatch(visitor)
    apply(visitor)
  }
}
