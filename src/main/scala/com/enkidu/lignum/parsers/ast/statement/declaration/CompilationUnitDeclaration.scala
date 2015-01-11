package com.enkidu.lignum.parsers.ast.statement.declaration

import com.enkidu.lignum.parsers.ast.statement.declaration.imports.ImportDeclaration
import com.enkidu.lignum.parsers.ast.statement.declaration.packages.PackageDeclaration
import com.enkidu.lignum.parsers.ast.statement.declaration.types.TypeDeclaration

case class CompilationUnitDeclaration(`package`: PackageDeclaration, imports: Seq[ImportDeclaration],
                                      declarations: Seq[TypeDeclaration]) extends Declaration {
  override def dispatch(visitor: Visitor) = {
    `package`.dispatch(visitor)
    imports.dispatch(visitor)
    declarations.dispatch(visitor)
    apply(visitor)
  }
}