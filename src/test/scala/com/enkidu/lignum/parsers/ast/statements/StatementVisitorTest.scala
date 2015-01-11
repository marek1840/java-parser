package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.conditional.{If, IfThenElse}
import com.enkidu.lignum.parsers.ast.statement.constructor._

class StatementVisitorTest extends VisitorTest {

  "Visitor should be properly dispatched in" - {
    "Block" in {
      Block(Vector()).visit()
      visited shouldBe 1
    }

    "EmptyStatement" in {
      EmptyStatement.visit()
      visited shouldBe 1
    }

    "LabeledStatement" in {
      LabeledStatement("", stmt).visit()
      visited shouldBe 2
    }

    "SynchronizedBlock" in {
      SynchronizedBlock(expr, block).visit()
      visited shouldBe 3
    }

    "Assertion" in {
      Assertion(expr, expr).visit()
      visited shouldBe 3
    }

    "conditional" - {
      "If" in {
        If(expr, stmt).visit()
        visited shouldBe 3
      }
      "IfThenElse" in {
        IfThenElse(expr, stmt, stmt).visit()
        visited shouldBe 4
      }
    }
    "constructor invocations" - {
      "AlternateConstructorInvocation" in {
        AlternateConstructorInvocation(arg, expr).visit()
        visited shouldBe 3
      }
      "IndirectParentConstructorInvocation" in {
        IndirectParentConstructorInvocation(expr, arg, expr).visit()
        visited shouldBe 4
      }
      "IntermidiateConstructorInvocation" in {
        IntermidiateConstructorInvocation(expr, arg, expr).visit()
        visited shouldBe 4
      }
      "ParentConstructorInvocation" in {
        ParentConstructorInvocation(arg, expr).visit()
        visited shouldBe 3
      }
    }


  }
}
