package com.enkidu.lignum.parsers.ast.statement.declaration

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.statement.declarator.Declarator

case class LocalVariableDeclaration(annotations: Seq[Annotation], isFinal: Boolean,
                                    `type`: Type, declarators: Seq[Declarator]) extends Declaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    `type`.dispatch(visitor)
    declarators.dispatch(visitor)
    apply(visitor)
  }
}