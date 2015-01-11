package com.enkidu.lignum.parsers.ast.expression.discardable.dimension

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class AbstractDimension(annotations: Seq[Annotation]) extends Dimension {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    apply(visitor)
  }
}

