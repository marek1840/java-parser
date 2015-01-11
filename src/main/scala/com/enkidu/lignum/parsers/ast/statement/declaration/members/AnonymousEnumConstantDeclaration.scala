package com.enkidu.lignum.parsers.ast.statement.declaration.members

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class AnonymousEnumConstantDeclaration(annotations: Seq[Annotation], name: String,
                                            constructorArguments: Seq[Expression], members: Seq[MemberDeclaration]) extends
MemberDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    constructorArguments.dispatch(visitor)
    members.dispatch(visitor)
    apply(visitor)
  }
}
