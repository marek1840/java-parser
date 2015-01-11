package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.loop._

class LoopVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "Iteration" in {
      Iteration(local, expr, stmt).visit()
      visited shouldBe 7
    }
    "DoWhile" in {
      DoWhile(stmt, expr).visit()
      visited shouldBe 3
    }
    "While" in {
      While(expr, stmt).visit()
      visited shouldBe 3
    }
    "For" in {
      For(stmt, expr, stmt, stmt).visit()
      visited shouldBe 5
    }
  }
}
