package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension

case class ArrayDeclarator(name: String, dimensions: Seq[Dimension]) extends Declarator {
  override def dispatch(visitor: Visitor): Unit = {
    dimensions.dispatch(visitor)
    apply(visitor)
  }
}