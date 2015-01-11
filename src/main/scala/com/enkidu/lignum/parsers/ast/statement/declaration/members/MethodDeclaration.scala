package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ClassType
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateParameter
import com.enkidu.lignum.parsers.ast.statement.Statement
import com.enkidu.lignum.parsers.ast.statement.declarator.{FunctionDeclarator, Declarator, MethodDeclarator}
import com.enkidu.lignum.parsers.ast.statement.modifers.Modifier

case class MethodDeclaration(annotations: Seq[Annotation], modifier: Seq[Modifier],
                             types: Seq[TemplateParameter], resultType: Type,
                             declarator: FunctionDeclarator, thrown: Seq[ClassType],
                             body: Statement) extends MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    types.dispatch(visitor)
    resultType.dispatch(visitor)
    declarator.dispatch(visitor)
    thrown.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}
