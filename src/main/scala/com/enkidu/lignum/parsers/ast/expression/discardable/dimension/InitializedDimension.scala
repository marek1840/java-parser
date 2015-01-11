package com.enkidu.lignum.parsers.ast.expression.discardable.dimension

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class InitializedDimension(annotations: Seq[Annotation], expr: Expression) extends Dimension {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    expr.dispatch(visitor)
    apply(visitor)
  }
}

