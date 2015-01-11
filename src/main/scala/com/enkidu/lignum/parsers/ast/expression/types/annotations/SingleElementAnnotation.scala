package com.enkidu.lignum.parsers.ast.expression.types.annotations

import com.enkidu.lignum.parsers.ast.expression.Expression

case class SingleElementAnnotation(name: Seq[String], value: Expression) extends Annotation {
  override def dispatch(visitor: Visitor): Unit = {
    value.dispatch(visitor)
    apply(visitor)
  }
}
