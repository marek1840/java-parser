package com.enkidu.lignum.parsers.ast.expression.types.annotations

import com.enkidu.lignum.parsers.ast.expression.Expression

case class NormalAnnotation(name: Seq[String], elements: Seq[(String, Expression)]) extends Annotation {
  override def dispatch(visitor: Visitor): Unit = {
    elements.foreach(_._2.dispatch(visitor))
    apply(visitor)
  }
}
