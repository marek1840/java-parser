package com.enkidu.lignum.parsers.ast.statement.declaration.packages

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class NamedPackageDeclaration(annotations: Seq[Annotation], name: Seq[String]) extends PackageDeclaration {
  override def dispatch(visitor: Visitor) = {
    annotations.dispatch(visitor)
    apply(visitor)
  }
}
