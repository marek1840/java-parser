package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.declarator._

class DeclaratorVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "VariableDeclarator" in {
      VariableDeclarator("").visit()
      visited shouldBe 1
    }
    "InitializedArrayDeclarator" in {
      InitializedArrayDeclarator("", dim, expr).visit()
      visited shouldBe 3
    }
    "MethodDeclarator" in {
      MethodDeclarator("", argParam).visit()
      visited shouldBe 2
    }
    "ArrayDeclarator" in {
      ArrayDeclarator("", dim).visit()
      visited shouldBe 2
    }
    "InitializedVariableDeclarator" in {
      InitializedVariableDeclarator("", expr).visit()
      visited shouldBe 2
    }
    "ArrayMethodDeclarator" in {
      ArrayMethodDeclarator("", argParam, dim).visit()
      visited shouldBe 3
    }
    "ConstructorDeclarator" in {
      ConstructorDeclarator("", argParam).visit()
      visited shouldBe 2
    }
  }
}
