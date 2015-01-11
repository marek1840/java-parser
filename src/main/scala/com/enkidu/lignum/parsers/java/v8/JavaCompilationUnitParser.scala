package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.statement.declaration.CompilationUnitDeclaration
import com.enkidu.lignum.parsers.ast.statement.declaration.imports._
import com.enkidu.lignum.parsers.ast.statement.declaration.packages.{NamedPackageDeclaration, PackageDeclaration, UnnamedPackageDeclaration}
import com.enkidu.lignum.parsers.ast.statement.declaration.types.TypeDeclaration
import org.parboiled2.{ParserInput, Rule1}

class JavaCompilationUnitParser(override val input:ParserInput) extends JavaInterfaceParser {
  def compilationUnit: Rule1[CompilationUnitDeclaration] = rule {
    whitespace ~ packageDeclaration ~ importDeclarations ~ typeDeclarations ~ EOI ~> CompilationUnitDeclaration
  }

  private def typeDeclarations: Rule1[Seq[TypeDeclaration]] = rule {
    zeroOrMore(typeDeclaration)
  }

  def typeDeclaration: Rule1[TypeDeclaration] = rule {
    classDeclaration |
      enumDeclaration |
      interfaceDeclaration |
      annotationDeclaration |
      emptyDeclaration
  }

  private def packageDeclaration: Rule1[PackageDeclaration] = rule {
    annotations ~ `package` ~ (oneOrMore(identifier) separatedBy dot) ~ semicolon ~> NamedPackageDeclaration |
      push(UnnamedPackageDeclaration)
  }

  def importDeclarations: Rule1[Seq[ImportDeclaration]] = rule {
    zeroOrMore {
      `import` ~ {
        `static` ~ {
          qualifiedIdentifier ~ {
            dot ~ `*` ~> StaticLazyImportDeclaration |
              MATCH ~> StaticImportDeclaration
          }
        } |
          qualifiedIdentifier ~ {
            dot ~ `*` ~> LazyImportDeclaration |
              MATCH ~> SingleImportDeclaration
          }
      } ~ semicolon
    }
  }
}
