package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ClassType
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateParameter
import com.enkidu.lignum.parsers.ast.statement.Block
import com.enkidu.lignum.parsers.ast.statement.declarator.ConstructorDeclarator
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class ConstructorDeclaration(annotations: Seq[Annotation], modifiers: Seq[Modifier],
                                  types: Seq[TemplateParameter], declarator: ConstructorDeclarator,
                                  thrown: Seq[ClassType], body: Block) extends MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    types.dispatch(visitor)
    declarator.dispatch(visitor)
    thrown.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}
