package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class AnnotationElementDeclaration(annotations: Seq[Annotation], modifier: Seq[Modifier],
                                        `type`: Type, name: String) extends MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    `type`.dispatch(visitor)
    apply(visitor)
  }
}
