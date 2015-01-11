package com.enkidu.lignum.parsers.ast.statement.parameter

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class InstanceReceiverParameter(annotations: Seq[Annotation], `type`: Type) extends Parameter {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    `type`.dispatch(visitor)
    apply(visitor)
  }
}