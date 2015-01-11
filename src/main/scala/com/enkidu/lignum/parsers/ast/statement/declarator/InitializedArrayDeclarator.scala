package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension

case class InitializedArrayDeclarator(name: String, dimensions: Seq[Dimension], initializer: Expression) extends Declarator {
  override def dispatch(visitor: Visitor): Unit = {
    dimensions.dispatch(visitor)
    initializer.dispatch(visitor)
    apply(visitor)
  }
}