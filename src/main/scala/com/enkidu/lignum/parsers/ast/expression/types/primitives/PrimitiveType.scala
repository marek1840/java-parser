package com.enkidu.lignum.parsers.ast.expression.types.primitives

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

trait PrimitiveType extends Type {
  def annotations: Seq[Annotation]

  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    apply(visitor)
  }
}











