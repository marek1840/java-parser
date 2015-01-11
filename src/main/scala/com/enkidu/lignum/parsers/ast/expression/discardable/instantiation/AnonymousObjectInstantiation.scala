package com.enkidu.lignum.parsers.ast.expression.discardable.instantiation

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument
import com.enkidu.lignum.parsers.ast.statement.declaration.members.MemberDeclaration


case class AnonymousObjectInstantiation(constructorTypes: Seq[TemplateArgument], `type`: Type,
                                        constructorArguments: Seq[Expression], body: Seq[MemberDeclaration]) extends ObjectInstantiation {
  override def dispatch(visitor: Visitor): Unit = {
    constructorTypes.dispatch(visitor)
    `type`.dispatch(visitor)
    constructorArguments.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}
