package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class AnnotationDefaultElementDeclaration(annotations: Seq[Annotation], modifier: Seq[Modifier],
                                               `type`: Type, name: String, default: Expression) extends MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    `type`.dispatch(visitor)
    default.dispatch(visitor)
    apply(visitor)
  }
}
