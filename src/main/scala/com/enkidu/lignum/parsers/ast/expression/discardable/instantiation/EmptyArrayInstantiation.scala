package com.enkidu.lignum.parsers.ast.expression.discardable.instantiation

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension
import com.enkidu.lignum.parsers.ast.expression.types.Type

case class EmptyArrayInstantiation(`type`: Type, dimensions: Seq[Dimension]) extends ArrayInstantiation {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    dimensions.dispatch(visitor)
    apply(visitor)
  }
}
