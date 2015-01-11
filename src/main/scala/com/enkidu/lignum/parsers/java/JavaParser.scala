package com.enkidu.lignum.parsers.java

import com.enkidu.lignum.parsers.ast.statement.declaration.Declaration
import com.enkidu.lignum.parsers.ast.statement.declaration.imports.ImportDeclaration
import com.enkidu.lignum.parsers.java.v8.JavaCompilationUnitParser
import org.parboiled2.ParserInput


object JavaParser {
  private def parser(input: String): JavaCompilationUnitParser = new JavaCompilationUnitParser(ParserInput(input))

  def parseCompilationUnit(input: String): Declaration = parser(input).compilationUnit.run().get
  def parseImports(input: String): Seq[ImportDeclaration] = parser(input).importDeclarations.run().get

  def parseClass(input: String): Declaration = parser(input).classDeclaration.run().get
  def parseClassMember(input: String): Declaration = parser(input).classMemberDeclaration.run().get

  def parseEnum(input: String): Declaration = parser(input).enumDeclaration.run().get
  def parseEnumBody(input: String): Seq[Declaration] = parser(input).enumBody.run().get

  def parseInterface(input: String): Declaration = parser(input).interfaceDeclaration.run().get
  def parseInterfaceMember(input: String): Declaration = parser(input).interfaceMemberDeclaration.run().get

  def parseAnnotation(input: String): Declaration = parser(input).annotationDeclaration.run().get
  def parseAnnotationMember(input: String): Declaration = parser(input).annotationMemberDeclaration.run().get

}
