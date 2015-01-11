package com.enkidu.lignum.parsers.ast.statements

import com.enkidu.lignum.parsers.ast.VisitorTest
import com.enkidu.lignum.parsers.ast.statement.interruptable._

class InterruptibleVisitorTest extends VisitorTest {
  "Visitor should be properly dispatched in" - {
    "CatchClause" in {
      CatchClause(local, block).visit()
      visited shouldBe 6
    }
    "TryCatch" in {
      TryCatch(block, CatchClause(local, block)).visit()
      visited shouldBe 8
    }
    "TryCatchFinally" in {
      TryCatchFinally(block, CatchClause(local, block), block).visit()
      visited shouldBe 9
    }
    "TryFinally" in {
      TryFinally(block, block).visit()
      visited shouldBe 3
    }
    "TryWithResources" in {
      TryWithResources(local, block, CatchClause(local, block), block).visit()
      visited shouldBe 13
    }
  }
}
