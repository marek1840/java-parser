package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.declaration._
import com.enkidu.lignum.parsers.ast.statement.declaration.imports._
import com.enkidu.lignum.parsers.ast.statement.declaration.initializers._
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.declaration.packages._
import com.enkidu.lignum.parsers.ast.statement.declaration.types._
import com.enkidu.lignum.parsers.ast.statement.declarator._

class DeclarationVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "CompilationUnitDeclaration" in {
      CompilationUnitDeclaration(UnnamedPackageDeclaration, SingleImportDeclaration(""), decl).visit()
      visited shouldBe 4
    }
    "LocalVariableDeclaration" in {
      LocalVariableDeclaration(ann, false, typ, VariableDeclarator("")).visit()
      visited shouldBe 4
    }
    "declarations" - {
      "import" - {
        "LazyImport" in {
          StaticLazyImportDeclaration("").visit()
          visited shouldBe 1
        }
        "SingleImport" in {
          StaticLazyImportDeclaration("").visit()
          visited shouldBe 1
        }
        "StaticImport" in {
          StaticLazyImportDeclaration("").visit()
          visited shouldBe 1
        }
        "StaticLazyImport" in {
          StaticLazyImportDeclaration("").visit()
          visited shouldBe 1
        }
      }
      "initializers" - {
        "InstanceInitializerDeclaration" in {
          InstanceInitializerDeclaration(block).visit()
          visited shouldBe 2
        }
        "StaticInitializerDeclaration" in {
          StaticInitializerDeclaration(block).visit()
          visited shouldBe 2
        }
      }
      "members" - {
        "AnnotationDefaultElementDeclaration" in {
          AnnotationDefaultElementDeclaration(ann, Nil, typ, "", expr).visit()
          visited shouldBe 4
        }
        "AnnotationElementDeclaration" in {
          AnnotationElementDeclaration(ann, Nil, typ, "").visit()
          visited shouldBe 3
        }
        "AnonymousEnumConstantDeclaration" in {
          AnonymousEnumConstantDeclaration(ann, "", expr, decl).visit()
          visited shouldBe 4
        }
        "ConstantDeclaration" in {
          ConstantDeclaration(ann, Nil, typ, VariableDeclarator("")).visit()
          visited shouldBe 4
        }
        "ConstructorDeclaration" in {
          ConstructorDeclaration(ann, Nil, templParam, ConstructorDeclarator("", argParam), typ, block).visit()
          visited shouldBe 7
        }
        "EnumConstantDeclaration" in {
          EnumConstantDeclaration(ann, "", expr).visit()
          visited shouldBe 3
        }
        "FieldDeclaration" in {
          FieldDeclaration(ann, Nil, typ, VariableDeclarator("")).visit()
          visited shouldBe 4
        }
        "InterfaceMethodDeclaration" in {
          InterfaceMethodDeclaration(ann, Nil, templParam, typ, MethodDeclarator("", argParam), typ, stmt).visit()
          visited shouldBe 8
        }
        "MethodDeclaration" in {
          MethodDeclaration(ann, Nil, templParam, typ, MethodDeclarator("", argParam), typ, stmt).visit()
          visited shouldBe 8
        }
      }
      "packages" - {
        "NamedPackageDeclaration" in {
          NamedPackageDeclaration(ann, "").visit()
          visited shouldBe 2
        }
        "UnnamedPackageDeclaration$" in {
          UnnamedPackageDeclaration.visit()
          visited shouldBe 1
        }
      }
      "types" - {
        "AnnotationDeclaration" in {
          AnnotationDeclaration(ann, Nil, "", decl).visit()
          visited shouldBe 3
        }
        "ClassDeclaration" in {
          ClassDeclaration(ann, Nil, "", templParam, typ, typ, decl).visit()
          visited shouldBe 6
        }
        "EmptyDeclaration" in {
          EmptyDeclaration.visit()
          visited shouldBe 1
        }
        "EnumDeclaration" in {
          EnumDeclaration(ann, Nil, "", typ, decl).visit()
          visited shouldBe 4
        }
        "InterfaceMethodDeclaration" in {
          InterfaceMethodDeclaration(ann, Nil, templParam, typ, MethodDeclarator("", argParam), typ, stmt).visit()
          visited shouldBe 8
        }
      }
    }
  }
}
