package com.enkidu.lignum.parsers.ast.statement.declaration.types

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ClassType
import com.enkidu.lignum.parsers.ast.statement.declaration.members.MemberDeclaration
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class EnumDeclaration(annotations: Seq[Annotation], modifiers: Seq[Modifier],
                           name: String, interfaces: Seq[ClassType], members: Seq[MemberDeclaration]) extends TypeDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    interfaces.dispatch(visitor)
    members.dispatch(visitor)
    apply(visitor)
  }
}
