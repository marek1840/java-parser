package com.enkidu.lignum.parsers.ast.expression.types.references

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension
import com.enkidu.lignum.parsers.ast.expression.types.Type

case class ArrayType(`type`: Type, dimensions: Seq[Dimension]) extends ReferenceType {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    dimensions.dispatch(visitor)
    apply(visitor)
  }
}
