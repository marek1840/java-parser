package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.statement.declarator.Declarator
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class FieldDeclaration(annotations: Seq[Annotation], modifiers: Seq[Modifier],
                            `type`: Type, declarators: Seq[Declarator]) extends MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    `type`.dispatch(visitor)
    declarators.dispatch(visitor)
    apply(visitor)
  }
}
